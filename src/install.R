versions <- list(
  list(package="dplyr", version="0.8.0.1"),
  list(package="purrr"),
  list(package="pscl", version="1.5.2"),
  list(package="gbm", version="2.1.5"),
  list(package="knitr"),
  list(package="roxygen2"),
  list(package="rgdal"),
  list(package="testthat")
)

for (version in versions) {
    do.call(devtools::install_version, version)
}
