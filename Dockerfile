FROM rocker/geospatial
WORKDIR /api_covid_statistics

# Install R packages from Ubuntu binaries where possible
RUN apt-get update && apt-get install -y -qq \
  r-cran-tidyverse \
  r-cran-readxl \
  r-cran-lubridate \
  r-cran-dbi \
  r-cran-httr \
  r-cran-devtools

# Install remaining R packages from source (install dev version of scanstatistics)
RUN R -e "install.packages(c('geojsonio', 'plumber', 'RPostgreSQL', 'PHEindicatormethods'), dependencies = T)"
RUN R -e "devtools::install_github('benjak/scanstatistics', ref = 'develop')"

# Copy over scripts and data needed to run R API
COPY ./CloakScanStats.R .
COPY ./RunCLOAK.R .

# Set environment variables
ARG PGDATABASE
ARG PGPORT
ARG PGPASSWORD
ENV PG_DATABASE=${PGDATABASE}
ENV PG_PORT=${PGPORT}
ENV PG_PASSWORD=${PGPASSWORD}
ARG SITE_URL
ENV SITE_URL=${SITE_URL}

# Release port for API call
EXPOSE 8085

# Run R API
ENTRYPOINT ["R", "-e", "source('RunCLOAK.R')"]
