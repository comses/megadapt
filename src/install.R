versions <- list(
  list(package="argparse"),
  list(package="assertr"),
  list(package="bindrcpp"),
  list(package="dplyr", version="0.8.0.1"),
  list(package="fs"),
  list(package="future.apply"),
  list(package="future.batchtools"),
  list(package="gbm", version="2.1.5"),
  list(package="knitr"),
  list(package="leaflet"),
  list(package="logging"),
  list(package="qrng"),
  list(package="pscl", version="1.5.2"),
  list(package="purrr"),
  list(package="rgdal"),
  list(package="rgeos"),
  list(package="roxygen2"),
  list(package="shiny"),
  list(package="testthat"),
  list(package="tidyr")
)

for (version in versions) {
    do.call(devtools::install_version, version)
}


