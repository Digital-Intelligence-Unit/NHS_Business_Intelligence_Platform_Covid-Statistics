#* Filter CORS otherwise Angular's API call gets blocked
#* If the request isn't just for options then check the user has a valid Nexus
#*   Intelligence JWT token and only forward into the actual API if they do
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader(
      "Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )
    res$status <- 200
    return(list())
  } else {
    if (!"referer" %in% names(req$HEADERS)) {
      authorised <- 401
    } else if (
      !any(str_detect(req$HEADERS["referer"], c("plumber", "swagger")))
    ) {
      jwt <- req$HEADERS["authorization"]
      authorised <- tryCatch({
        GET(
          Sys.getenv('SITE_URL'),
          add_headers(authorization = unname(jwt))
        ) %>%
          status_code()
      },
      error = function(e) {
        return(401)
      })
    } else {
      authorised <- 200
    }

    if (authorised == 200) {
      plumber::forward()
    } else {
      return(list())
    }
  }
}

# Connect to postgres DB and run query, then disconnect
query_postgres <- function(
  query,
  is_geom_query = FALSE,
  geometry_column = 'geometry'
) {
  postgres <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = Sys.getenv('PG_DATABASE'),
    dbname = 'postgres',
    user = 'postgres',
    password = Sys.getenv('PG_PASSWORD'),
    port = Sys.getenv('PG_PORT')
  )

  # Run PostgreSQL query
  if (is_geom_query) {
    # If we want geometry objects returned
    result <- st_read(
      postgres,
      query = query
    )
  } else {
    # If we want to run a normal query
    result <- dbGetQuery(postgres, query)
  }

  dbDisconnect(postgres)

  result
}

get_cloak_data <- function(
  start_date,
  end_date,
  data_type = 'Testing',
  geo_level = 'lsoa',
  is_geom = FALSE,
  geom_column = 'geom'
) {
  cat('\nGetting', data_type, 'Data...')

  # Load test results and get positive daily count by geo-level
  query <- case_when(
    geo_level == 'lsoa' ~
      str_replace_all(
        "WITH daily_lsoa_count AS (
             SELECT
               specimen_date AS date,
               lsoa,
               COUNT(*) AS positives
             FROM
               covid19_cases_p1p2 c
             JOIN postcode_to_lsoa p ON
               c.postcodenowhite = p.postcode
             WHERE
               specimen_date >= '<start_date>'
               AND specimen_date <= '<end_date>'
             GROUP BY
               date, lsoa
           )
           SELECT
             date,
             l.lsoa,
             COALESCE(positives, 0) AS positives,
             lat,
             long,
             population AS lancs_pop
           FROM
             daily_lsoa_count d
           RIGHT JOIN lsoa_shapes l ON
             d.lsoa = l.lsoa
           ORDER BY
             d.date, d.lsoa;",
        c('<start_date>' = start_date, '<end_date>' = end_date)
      ),
    str_detect(geo_level, 'ward') ~
      str_replace_all(
        "SELECT
           specimen_date AS date,
           geom
         FROM
           covid19_cases_p1p2
         WHERE
           specimen_date >= '<start_date>'
           AND specimen_date <= '<end_date>';",
        c(
          '<start_date>' = start_date,
          '<end_date>' = end_date
        )
      ),
    TRUE ~
      str_replace_all(
        "WITH daily_area_count AS (
             SELECT
               specimen_date AS date,
               p.<geo_level>,
               COUNT(*) AS positives
             FROM
               covid19_cases_p1p2 c
             JOIN postcode_to_utla p ON
               c.postcodenowhite = p.postcode
             WHERE
               specimen_date >= '<start_date>'
               AND specimen_date <= '<end_date>'
             GROUP BY
               date, p.<geo_level>
          ),
          lsoa_to_geo_level AS (
            SELECT DISTINCT
              lsoa,
              <geo_level>
            FROM
              postcode_to_utla
            WHERE
              <geo_level> IN (
                SELECT DISTINCT <geo_level> FROM daily_area_count
              )
          )
          SELECT
            date,
            l.lsoa,
            p.<geo_level>,
            COALESCE(positives, 0) AS positives,
            lat,
            long,
            population AS lancs_pop
          FROM
            daily_area_count d
          JOIN lsoa_to_geo_level p ON
            p.<geo_level> = d.<geo_level>
          JOIN lsoa_shapes l ON
            l.lsoa = p.lsoa
          ORDER BY
            l.lsoa;",
        c(
          '<start_date>' = start_date,
          '<end_date>' = end_date,
          '<geo_level>' = geo_level
        )
      )
  )

  if (is_geom) {
    suppressMessages(
      lancs_daily_tests <- ward_shapefile %>%
        st_join(query_postgres(query, is_geom, geom_column)) %>%
        as_tibble() %>%
        group_by(date, area = code) %>%
        summarise(
          positives = n(),
          lancs_pop = first(lancs_pop)
        ) %>%
        ungroup() %>%
        mutate(positives = if_else(is.na(date), 0L, positives))
    )
  } else {
    lancs_daily_tests <- query_postgres(query) %>%
      mutate(area = !! sym(geo_level))
  }

  cat(' Done!\n\n')

  # Either return the raw SQL data output, dataframe of unique LSOAs, or
  #   transformed dataframe for model runs
  if(data_type == 'SQL') {
    return(lancs_daily_tests)
  } else if (data_type == 'AREA') {
    all_areas <- lancs_daily_tests %>%
      distinct(area) %>%
      mutate(id = 1)

    return(all_areas)
  } else {
    cat('\nTransforming', data_type, 'Data...')

    all_areas <- lancs_daily_tests %>%
      distinct(area) %>%
      mutate(id = 1)

    # Get area cases by two-day intervals
    suppressMessages(
      lancs_counts <- lancs_daily_tests %>%
        mutate(
          date_group = floor(yday(date) / 2) * 2,
          date_group = coalesce(date_group, min(date_group, na.rm = TRUE))
        ) %>%
        group_by(date_group, area) %>%
        summarise(positives = first(positives)) %>%
        ungroup()
    )

    all_dates <- lancs_counts %>%
      pull(date_group) %>%
      unique()

    all_dates <- data.frame(
      date_group = seq(min(all_dates), max(all_dates))
    ) %>%
      mutate(id = 1)

    full_area_date <- all_dates %>% full_join(all_areas, by = 'id')

    lancs_counts <- full_area_date %>%
      left_join(
        select(lancs_counts, -date_group, -positives),
        by = 'area'
      ) %>%
      distinct() %>%
      left_join(
        select(lancs_counts, area, date_group, positives),
        by = c('area', 'date_group')
      ) %>%
      mutate(positives = coalesce(positives, 0L)) %>%
      left_join(
        distinct(lancs_daily_tests, area, lancs_pop),
        by = 'area'
      ) %>%
      drop_na()

    suppressMessages(
      lancs_counts <- lancs_counts %>%
        group_by(area, date_group) %>%
        summarise(
          positives = first(positives),
          lancs_pop = sum(lancs_pop)
        ) %>%
        ungroup()
    )

    cat(' Done!\n\n')

    return(lancs_counts)
  }
}

run_cloak <- function(date_start, date_end, geo_level) {
  cat('\nGetting Data...')

  lancs_counts <- get_cloak_data(
    date_start,
    date_end,
    geo_level = geo_level,
    is_geom = geo_level == 'ward'
  )
  lancs_training_counts <- get_cloak_data(
    '2019-01-01',
    as.character(ymd(date_start) - 1),
    'Training',
    geo_level = geo_level,
    is_geom = geo_level == 'ward'
  )

  cat('\nRunning Model...')

  poisson_model <- glm(
    positives ~ offset(log(lancs_pop)) + 1 + I(date_group - min(date_group)),
    family = poisson(link = 'log'),
    data = lancs_training_counts
  )

  prediction <- predict(
    poisson_model,
    newdata = lancs_counts,
    type = 'response'
  )

  lancs_baselines <- lancs_counts %>%
    transmute(
      time = date_group,
      location = area,
      count = positives,
      baseline = coalesce(prediction, 0)
    )

  # Split lancashire into KNN zones by lat-long
  # Find 10 nearest neighbours (or a max of ceiling(n-areas / 2))
  if (geo_level == 'ward') {
    lancs_area_query <- ward_shapefile %>%
      st_centroid() %>%
      select(code) %>%
      as_tibble() %>%
      rowwise() %>%
      mutate(
        lat = unlist(geometry)[1],
        long = unlist(geometry)[2]
      ) %>%
      select(area = code, lat, long)
  } else {
    lancs_area_query <- get_cloak_data(
      date_start,
      date_end,
      'SQL',
      geo_level = geo_level
    )
  }

  suppressMessages(
    lancs_zones <- lancs_area_query %>%
      group_by(area) %>%
      summarise(
        lat = mean(lat),
        long = mean(long),
        positives = n()
      ) %>%
      ungroup()
  )

  lancs_zones <- lancs_zones %>%
    distinct(long, lat) %>%
    as.matrix() %>%
    spDists(x = ., y = ., longlat = TRUE) %>%
    dist_to_knn(
      k = case_when(
        # nrow(lancs_zones) > 100 ~ 10,
        nrow(lancs_zones) > 50 ~ 10,
        TRUE ~ ceiling(nrow(lancs_zones) / 2)
      )
    ) %>%
    knn_zones()

  suppressMessages(
    lancs_areas <- lancs_area_query %>%
      group_by(area) %>%
      summarise(positives = ifelse(geo_level == 'ward', n(), sum(positives))) %>%
      ungroup()
  )

  # Remove any zones which have no cases within them (only for LSOAs & Wards)
  if (geo_level %in% c('lsoa')) {
    suppressMessages(
      any_in_zone <- lapply(
        lancs_zones,
        function(zone, area) {
          sum(area[zone]) > 0
        },
        area = lancs_areas %>% pull(positives)
      ) %>%
        unlist()
    )

    lancs_zones <- lancs_zones[any_in_zone]
  }

  cat(' Done!\n\n')
  cat('\nCalculating Area Statistics...')

  # Calculate the Poisson-scan statistic
  poisson_scan <- scan_eb_poisson(
    counts = lancs_baselines,
    zones = lancs_zones,
    n_mcsim = 1e2
  )

  cat(' Done!\n\n')
  cat('\nScoring Areas...')

  suppressMessages(
    zone_scores <- poisson_scan$observed %>%
      filter(score > 0) %>%
      group_by(zone) %>%
      summarise(sum_score = max(score)) %>%
      arrange(-sum_score)
  )

  lsoa_scores <- data.frame(
    area = lancs_areas$area,
    score = 0
  )

  invisible(apply(
    zone_scores,
    1,
    function(zone, zones) {
      areas <- zones[[zone['zone']]]
      lsoa_scores[areas,]$score <<- lsoa_scores[areas,]$score + zone['sum_score']
    },
    zones = lancs_zones
  ))

  lsoa_scores <- lsoa_scores %>%
    mutate(
      total_score = score,
      score = coalesce(total_score / length(lancs_zones)),
      relative_score = score / max(score)
    )

  cat(' Done!\n\n')
  cat('\nCreating GeoJSON\n\n...')

  if (str_detect(geo_level, 'lsoa') | is.null(geo_level)) {
    score_map <- lsoa_shapefile %>%
      right_join(lsoa_scores, by = c('lsoa' = 'area')) %>%
      rename(area = lsoa) %>%
      st_as_sf()
  } else if (str_detect(geo_level, 'ward')) {
    score_map <- ward_shapefile %>%
      filter(la %in% get_local_authorities()) %>%
      left_join(lsoa_scores, by = c('code' = 'area')) %>%
      mutate(
        score = coalesce(score, 0),
        total_score = coalesce(total_score, 0),
        relative_score = coalesce(relative_score, 0)
      ) %>%
      st_as_sf()
  } else {
    lancs_area_query <- lancs_area_query %>% distinct(lsoa, area)
    lsoa_scores <- lsoa_scores %>% left_join(lancs_area_query, by = 'area')
    suppressMessages(
      score_map <- lsoa_shapefile %>%
        right_join(lsoa_scores, by = 'lsoa') %>%
        group_by(area) %>%
        summarise(
          score = first(score),
          relative_score = first(relative_score),
          geometry = st_combine(geometry)
        ) %>%
        ungroup() %>%
        st_union(by_feature = TRUE) %>%
        st_as_sf()
    )

    if (geo_level == 'msoa') {
      msoa_names <- query_postgres(
        'SELECT msoa, msoa_nice_name FROM msoa_profiles;'
      )

      score_map <- score_map %>%
        left_join(msoa_names, by = c('area' = 'msoa'))
    }
  }

  score_map <- score_map %>%
    geojson_json()

  cat(' Done!\n\n')

  rm(lsoa_scores)

  score_map
}

# SQL to get crude rate for cases within time period, at geography level
get_crude_rate <- function(
  start_date,
  end_date,
  level_geo,
  is_geom = FALSE
) {
  geom_column <- 'geom'
  query <- case_when(
    str_detect(level_geo, 'lsoa') ~
      str_replace_all(
        "WITH daily_lsoa_count AS (
           SELECT
             lsoa,
             COUNT(*) AS positives,
             SUM(
               CASE WHEN linked_to_care_home = 'Y' THEN 1 ELSE 0 END
             ) AS count_in_care_home
           FROM
             covid19_cases_p1p2 c
           JOIN postcode_to_utla p ON
             c.postcodenowhite = p.postcode
           WHERE
             specimen_date >= '<start_date>'
             AND specimen_date <= '<end_date>'
           GROUP BY
             lsoa
         )
         SELECT
           d.*,
           population AS lancs_pop,
           (1000 * COALESCE(positives, 0)) :: double precision /
             (population) :: double precision AS crude_rate_per_thousand
         FROM
           daily_lsoa_count d
         RIGHT JOIN lsoa_shapes l ON
           d.lsoa = l.lsoa
         ORDER BY
           d.lsoa;",
        c(
          '<start_date>' = start_date,
          '<end_date>' = end_date
        )
      ),
    str_detect(level_geo, 'ward') ~
      str_replace_all(
        "SELECT
           linked_to_care_home AS care_home,
           geom
         FROM
           covid19_cases_p1p2
         WHERE
           specimen_date >= '<start_date>'
           AND specimen_date <= '<end_date>';",
        c(
          '<start_date>' = start_date,
          '<end_date>' = end_date
        )
      ),
    TRUE ~
      str_replace_all(
      "WITH daily_lsoa_count AS (
         SELECT
           lsoa,
           COUNT(*) AS positives,
           SUM(
             CASE WHEN linked_to_care_home = 'Y' THEN 1 ELSE 0 END
           ) AS count_in_care_home
         FROM
           covid19_cases_p1p2 c
         JOIN postcode_to_utla p ON
           c.postcodenowhite = p.postcode
         WHERE
           specimen_date >= '<start_date>'
           AND specimen_date <= '<end_date>'
         GROUP BY
           lsoa
       ),
       lsoa_to_geo_level AS (
         SELECT DISTINCT
           lsoa,
           <geo_level>
         FROM
           postcode_to_utla
       )
       SELECT
         l.lsoa,
         p.<geo_level>,
         COALESCE(positives, 0) AS positives,
         COALESCE(count_in_care_home, 0) AS count_in_care_home,
         population AS lancs_pop,
         (1000 * COALESCE(positives, 0)) :: double precision /
           (population) :: double precision AS crude_rate_per_thousand
       FROM
         daily_lsoa_count d
       RIGHT JOIN lsoa_shapes l ON
         l.lsoa = d.lsoa
       LEFT JOIN lsoa_to_geo_level p ON
         p.lsoa = l.lsoa
       ORDER BY
         l.lsoa;",
      c(
        '<start_date>' = start_date,
        '<end_date>' = end_date,
        '<geo_level>' = level_geo
      )
    )
  )

  query_postgres(query, is_geom, geom_column)
}

# Calculate crude rates per LSOA
run_cloak_crude <- function(date_start, date_end, geo_level) {
  n_weeks <- ( ymd(date_start) %--% (ymd(date_end) + 1) ) / dweeks()

  cat('\nGetting Data\n\n')
  lancs_crude_rate <- get_crude_rate(
    date_start,
    date_end,
    geo_level,
    geo_level == 'ward'
  )
  previous_crude_rate <- get_crude_rate(
    start_date = as.character(ymd(date_start) - n_weeks * 7),
    end_date = as.character(ymd(date_end) - n_weeks * 7),
    level_geo = geo_level,
    is_geom = geo_level == 'ward'
  )

  if (geo_level == 'ward') {
    suppressMessages(
      lancs_crude_rate <- lancs_crude_rate %>%
        st_join(ward_shapefile) %>%
        group_by(code) %>%
        summarise(
          positives = n(),
          crude_rate_per_thousand = 1e5 * positives / first(lancs_pop) / n_weeks,
          count_in_care_home = sum(care_home == 'Y'),
          care_home_ratio = sum(care_home == 'Y') / n()
        ) %>%
        ungroup() %>%
        select(
          lsoa = code,
          positives,
          crude_rate_per_thousand,
          count_in_care_home,
          care_home_ratio
        ) %>%
        as_tibble()
    )

    if ('geom' %in% names(lancs_crude_rate)) {
      lancs_crude_rate <- select(lancs_crude_rate , -geom)
    } else {
      lancs_crude_rate <- select(lancs_crude_rate , -geometry)
    }

    suppressMessages(
      previous_crude_rate <- tryCatch({
          previous_crude_rate %>%
            st_join(ward_shapefile) %>%
            group_by(code) %>%
            summarise(
              positives = n(),
              crude_rate_per_thousand = 1e5 * positives / first(lancs_pop) / n_weeks,
              count_in_care_home = sum(care_home == 'Y'),
              care_home_ratio = sum(care_home == 'Y') / n()
            ) %>%
            ungroup() %>%
            select(
              lsoa = code,
              positives,
              crude_rate_per_thousand,
              count_in_care_home,
              care_home_ratio
            ) %>%
            as_tibble()
        },
        error = function(e) {
          c(
            'lsoa' = NA,
            positives = NA,
            crude_rate_per_thousand = NA,
            count_in_care_home = NA,
            care_home_ratio = NA,
            geometry = NA
          ) %>%
            t() %>%
            as.data.frame()
        }
      )
    )

    if ('geom' %in% names(previous_crude_rate)) {
      previous_crude_rate <- select(previous_crude_rate , -geom)
    } else {
      previous_crude_rate <- select(previous_crude_rate , -geometry)
    }
  }

  previous_crude_rate <- previous_crude_rate %>%
    select(
      lsoa,
      previous_cases = positives,
      previous_crude_rate_per_thousand = crude_rate_per_thousand,
      previous_count_in_care_home = count_in_care_home
    )

  lancs_crude_rate <- lancs_crude_rate %>%
    full_join(previous_crude_rate, by = 'lsoa') %>%
    filter(!is.na(lsoa))

  cat('\nCreating GeoJSON...')

  if (str_detect(geo_level, 'soa')) {
    districts <- query_postgres(
      'SELECT DISTINCT
         lsoa,
         la
       FROM
         postcode_to_utla;'
    )

    lancs_crude_rate <- lancs_crude_rate %>% left_join(districts, by = 'lsoa')

    suppressMessages(
      lancs_districts <- lancs_crude_rate %>%
        group_by(area = !! sym(geo_level)) %>%
        summarise(la = first(la)) %>%
        ungroup()
    )
  }

  if (geo_level == 'ward') {
    crude_rate_map <- ward_shapefile %>%
      left_join(lancs_crude_rate, by = c('code' = 'lsoa'))
  } else {
    crude_rate_map <- lsoa_shapefile %>%
      left_join(lancs_crude_rate, by = 'lsoa')
  }

  if (str_detect(geo_level, 'ward')) {
    cat(' Aggregating to', geo_level, '...')
    suppressMessages(
      crude_rate_map <- crude_rate_map %>%
        filter(positives > 0 | previous_cases > 0) %>%
        mutate(
          positives = coalesce(positives, 0L),
          crude_rate_per_thousand = coalesce(crude_rate_per_thousand, 0),
          count_in_care_home = coalesce(count_in_care_home, 0L),
          care_home_ratio = coalesce(care_home_ratio, 0),
          previous_cases = coalesce(previous_cases, 0L),
          previous_crude_rate_per_thousand = coalesce(
            previous_crude_rate_per_thousand, 0L
          ),
          previous_count_in_care_home = coalesce(
            previous_count_in_care_home, 0L
          ),
          previous_care_home_ratio = coalesce(
            na_if(previous_count_in_care_home / previous_cases, Inf), 0
          )
        ) %>%
        st_as_sf()
    )
  } else if (!str_detect(geo_level, 'lsoa')) {
    cat(' Aggregating to', geo_level, '...')
    suppressMessages(
      crude_rate_map <- crude_rate_map %>%
        group_by(area = !! sym(geo_level)) %>%
        summarise(
          # Current period counts & rate
          positives = sum(coalesce(positives, 0L)),
          lancs_pop = sum(coalesce(lancs_pop, 1L)),
          crude_rate_per_thousand = 1e5 * positives / lancs_pop / n_weeks,

          # Previous period counts & rate
          previous_cases = sum(coalesce(previous_cases, 0L)),
          previous_crude_rate_per_thousand = 1e5 * previous_cases /
            lancs_pop / n_weeks,

          # Current and previous care home counts & ratio
          count_in_care_home = sum(coalesce(count_in_care_home, 0L)),
          previous_count_in_care_home = sum(coalesce(
            previous_count_in_care_home, 0L
          )),
          care_home_ratio = coalesce(
            na_if(count_in_care_home / positives, Inf), 0
          ),
          previous_care_home_ratio = coalesce(
            na_if(previous_count_in_care_home / previous_cases, Inf), 0
          ),

          # Combine geometry polygons
          geometry = st_combine(geometry)
        ) %>%
        ungroup() %>%
        st_union(by_feature = TRUE) %>%
        filter(positives > 0 | previous_cases > 0) %>%
        st_as_sf()
    )
  } else {
    crude_rate_map <- crude_rate_map %>%
      mutate(
        # Count and rate
        area = lsoa,
        positives = coalesce(positives, 0L),
        crude_rate_per_thousand = 100 *
          coalesce(crude_rate_per_thousand, 0) / n_weeks,

        # Previous period counts & rate
        previous_cases = coalesce(previous_cases, 0L),
        previous_crude_rate_per_thousand = 100 *
          coalesce(previous_crude_rate_per_thousand, 0) / n_weeks,

        # Current and previous care home counts & ratio
        count_in_care_home = coalesce(count_in_care_home, 0L),
        previous_count_in_care_home = coalesce(previous_count_in_care_home, 0L),
        care_home_ratio = coalesce(
          na_if(count_in_care_home / positives, Inf), 0
        ),
        previous_care_home_ratio = coalesce(
          na_if(previous_count_in_care_home / previous_cases, Inf), 0
        )
      ) %>%
      filter(positives > 0 | previous_cases > 0) %>%
      select(-lsoa) %>%
      st_as_sf()
  }

  crude_rate_map <- crude_rate_map %>%
    mutate(scaled_population = ceiling(lancs_pop * n_weeks)) %>%
    as_tibble() %>%
    phe_rate(positives, scaled_population)

  if (!str_detect(geo_level, 'la')) {
    if (geo_level == 'msoa') {
      crude_rate_map <- crude_rate_map %>%
        left_join(lancs_districts, by = 'area')

      msoa_names <- query_postgres(
        'SELECT msoa, msoa_nice_name FROM msoa_profiles;'
      )

      crude_rate_map <- crude_rate_map %>%
        left_join(msoa_names, by = c('area' = 'msoa'))
    }


    if (str_detect(geo_level, 'soa')) {
      suppressMessages(
        districts_map <- lsoa_shapefile %>%
          left_join(districts, by = 'lsoa') %>%
          group_by(la) %>%
          summarise(geometry = st_combine(geometry)) %>%
          ungroup() %>%
          mutate(district_boundary = 'black', crude_rate_per_thousand = 0) %>%
          st_union(by_feature = TRUE)
      )
    } else {
      keep_districts <- get_local_authorities()

      suppressMessages(
        districts_map <- ward_shapefile %>%
          filter(la %in% keep_districts) %>%
          group_by(la) %>%
          summarise(geometry = st_combine(geometry)) %>%
          ungroup() %>%
          mutate(district_boundary = 'black', crude_rate_per_thousand = 0) %>%
          st_union(by_feature = TRUE)
      )
    }

    crude_rate_map <- districts_map %>%
      bind_rows(crude_rate_map)

    crude_rate_map <- crude_rate_map %>%
      mutate(
        value = if_else(is.na(district_boundary), coalesce(value, 0), value),
        uppercl = if_else(is.na(district_boundary), coalesce(uppercl, 0), uppercl),
        lowercl = if_else(is.na(district_boundary), coalesce(lowercl, 0), lowercl)
      )
  }

  crude_rate_map <- crude_rate_map %>%
    mutate(
      rate_diff = coalesce(
        crude_rate_per_thousand - previous_crude_rate_per_thousand, 0
      ),
      care_home_ratio = round(100 * care_home_ratio, 2),
      previous_care_home_ratio = round(100 * previous_care_home_ratio, 2)
    )

  crude_rate_map <- crude_rate_map %>%
    st_as_sf() %>%
    geojson_json()

  cat(' Done!\n\n')

  crude_rate_map
}

get_local_authorities <- function() {
  query_postgres('SELECT DISTINCT la FROM postcode_to_utla;') %>% pull(la)
}

#' @param date_start
#' @param date_end
#' @param geo_level
#' @get /cloak
function(
  date_start = '2020-01-01',
  date_end = '2021-01-01',
  geo_level = 'lsoa'
) {
  run_cloak(date_start, date_end, geo_level)
}

#' @param date_start
#' @param date_end
#' @param geo_level
#' @get /covid_crude_rate
function(
  date_start = '2020-01-01',
  date_end = '2021-01-01',
  geo_level = 'lsoa'
) {
  run_cloak_crude(date_start, date_end, geo_level)
}

#' @get /reset_lsoa_polygons
function() {
  cat('\nGetting LSOA Shapes...')
  lsoa_shapefile <<- query_postgres(
    query = 'SELECT lsoa, geometry FROM lsoa_shapes;',
    is_geom_query = TRUE
  )
  cat(' Done!\n\n')
}

#' @get /reset_ward_polygons
function() {
  cat('\nGetting Ward Shapes...')
  ward_shapefile <<- query_postgres(
    query = 'SELECT
               ward_code AS code,
               ward_name AS area,
               population AS lancs_pop,
               lat,
               long,
               local_authority AS la,
               geometry
             FROM
               wards_2018;',
    is_geom_query = TRUE
  )
  cat(' Done!\n\n')
}

#' @get /health_check
function() {
  return("Uh, had a slight weapons malfunction. But, uh, everything's perfectly
  all right now. We're fine. We're all fine here, now, thank you. How are you?")
}
