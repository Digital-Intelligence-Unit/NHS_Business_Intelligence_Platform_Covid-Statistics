# COVID Statistics API

Calculating COVID statistics for a range of geographical areas, from LSOA up to Upper-Tier Local Authority. Both crude rates (COVID cases per 100 000 population) and area risk score (a spatio-temporal scan-statistics model) can be returned, along with methods to re-load the LSOA and Electoral Ward shapefiles, and a general health-check method.

## Running The Project

The project file `api-covid-statistics.Rproj` should be opened; this is needed to make sure the file paths are correct. The project can then be run in R via:

```r
source("~/RunCLOAK.R")
```

This will load into memory the latest LSOA and Electoral Ward shapefiles, and expose port `8085` to allow calls to be made. This first data load will be made without any restrictions, but to make any further calls the request header must include a JSON Web Token (JWT) which can be passed to the BI Platform authentication API (`auth.*/users/validate`) to validate user access to the data.

### Area Crude Rates

Crude Rates can be calculated to find rates per area (LSOA, MSOA, Ward, LTLA, or UTLA) within a specified date-range via `*/covid_crude_rate?date_start=...&date_end=...&geo_level=...`. The three parameters which are needed for this method are:

- `date_start` ~ Start date (inclusive) [yyyy-mm-dd],
- `date_end` ~ End date (inclusive) [yyyy-mm-dd],
- `geo_level` ~ Geography level, one of `(lsoa, msoa, ward, la, utla)`.

A GeoJSON is returned with each area's polygon, along with the number of positive cases within the area, rate within the area, modified rate (rate / number of weeks), confidence intervals, change in rate (compared to preceeding time-window), and area population.

### Run Scan-Statistics Model

Each area's risk score can be returned via `*/cloak?date_start=...&date_end=...&geo_level=...`, where the parameters are the same as in the crude-rate method.

The method uses a spatio-temporal scan-statistic to highlight clusters of areas with significantly more cases than expected, for a sustained period. This is calculated using an R package `scanstatistics`, available from CRAN.

First, areas are clustered into zones by centroid distance. This is done by:

- One geography (e.g. LSOA) is selected.
- The geography and its closest neighbour (by population-weighted centroid position) are grouped into zone 1.
- The geography and its closest 2 neighbours are grouped into zone 2.
- ...
- The geography and its closest $`k`$ neighbours are grouped into zone $`k`$.
- This process is performed for all $`k`$ geographies until $`k`$ zones have been created around each geography, forming $`n x k`$ zones.

Within each geography $`x`$, the expected number of cases ($`Y`$) at time $`t`$ is estimated by assuming cases are drawn from a Poisson distribution, as

```math
Y_{xt} \sim \mathrm{Poisson}\left(\mu_{xt}\right),
```

where the Poisson model used to estimate case numbers depends upon the local population, to account for areas of higher density.

The expected number of cases $`Y_{xt}`$ and observed number of cases $`y_{xt}`$ are aggregated for each zone and compared for each time-window, to look for zones with sustained increases in observed cases compared with expected cases. This is run 5000 times, sampling expected numbers of cases from the fitted Poisson distribution, to build up p-values, and each zone's score is given as the likelihood ratio comparing to the null hypothesis.

Each geography is then re-scored by summing the scores of each zone the geography appears in, and dividing by the number of zones and maximum time duration considered. The relative score is calculated by dividing each geography's score by the maximum geography score attained.

As with the crude-rate method, a GeoJSON is returned with each area's polygon, along with the area's risk score and relative score (risk score / maximum score).

### Refresh Shapefiles

Shapefiles are stored in the AWS postgres databases (prod & dev), and so if they are updated on the database side then they won't automatically refresh within this plumbeR api. To update the shapefiles without having to re-build the plumbeR container, two methods can be called:

- `*/reset_lsoa_polygons` ~ Re-pulls the LSOA shapefile from Postgres,
- `*/reset_ward_polygons` ~ Re-pulls the Electoral Ward shapefile from Postgres.

## Terms of Use

This specific code repository and all code within is © Crown copyright and available under the terms of the Open Government 3.0 licence.

The code has been developed and is maintained by the NHS and where possible we will try to adhere to the NHS Open Source Policy (<https://github.com/nhsx/open-source-policy/blob/main/open-source-policy.md>).

It shall remain free to the NHS and all UK public services.

### Contributions

This code has been authored by colleagues in the Digital Intelligence Unit @ NHS Blackpool CCG.

_This project and all code within is © Crown copyright and available under the terms of the Open Government 3.0 licence._
