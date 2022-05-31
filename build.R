install.packages(c('geojsonio', 'plumber', 'RPostgreSQL', 'PHEindicatormethods'), dependencies = T)
library(devtools)
url <- 'https://cran.r-project.org/src/contrib/Archive/reliaR/reliaR_0.01.tar.gz'
pkgFile <- 'reliaR_0.01.tar.gz'
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type='source', repos=NULL)
unlink(pkgFile)
devtools::install_github('benjak/scanstatistics', ref = 'develop')