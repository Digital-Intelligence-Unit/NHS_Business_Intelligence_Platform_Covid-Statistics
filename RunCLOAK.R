need_packages <- c(
  'PHEindicatormethods',
  'RPostgreSQL',
  'readxl',
  'rgdal',
  'lubridate',
  'sf',
  'sp',
  'scanstatistics',
  'geojsonio',
  'plumber',
  'httr',
  'tidyverse'
)

lapply(need_packages, library, character.only = TRUE)

#' @apiTitle CLOAK Spatio-Temporal Scan Statistics Model
#' @apiDescription COVID-19 local outbreak detection using a spatio-temporal scan-statistics model, originally formulated by ![Kulldorf (1996)](https://www.tandfonline.com/doi/abs/10.1080/03610929708831995), and extended to use an expectation-based Poisson model by ![Neil et al. (2005)](https://www.cs.cmu.edu/~neill/papers/ijf.pdf). This is implemented in R by ![Allevius (2018)](https://cran.r-project.org/web/packages/scanstatistics/vignettes/introduction.html).
#' @apiVersion 1.0.0

source('CloakScanStats.R')

options(stringsAsFactors = FALSE)

if (!exists('lsoa_shapefile')) {
  cat('\nGetting LSOA Shapes...')
  lsoa_shapefile <<- query_postgres(
    query = 'SELECT lsoa, geometry FROM lsoa_shapes;',
    is_geom_query = TRUE
  )
  cat(' Done!\n\n')
}

if (!exists('ward_shapefile')) {
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

r <- plumb('CloakScanStats.R')
r$run(host = '0.0.0.0', port = 8085, swagger = TRUE)
