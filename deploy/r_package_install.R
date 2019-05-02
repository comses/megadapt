read_package <- function(file_name) {
  versions <- read.csv(file_name, stringsAsFactors = F)
  apply(
    versions,
    MARGIN = 1,
    FUN = function(row) {
      lst <- as.list(row)
      lapply(lst, function(el) {
        if (all(is.na(el))) {
          NULL
        } else {
          el
        }
      })
    }
  )
}

install <- function() {
  manifest <- read_package('r_package_manifest.csv')

  lapply(manifest, function(args) {
    do.call(devtools::install_version, args)
  })
}

install()

