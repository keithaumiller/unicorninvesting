source('util/is.R')

#' install.packages
#'
#' Installs packages only if the package has not been installed.
#' @param packages vector of package names
#' @param string mirror
#' @param dependencies if TRUE, installs package dependencies; defaults to FALSE
#' @examples
#' install.packages(c("ggplot2", "RMySQL"), "http://cran.us.r-project.org")
install.package <- function (packages, mirror, dependencies = FALSE) {
  for (i in 1:length(packages)) {
    package <- packages[i]

    if ( !is.installed(package) ) {
      install.packages(package, repos = mirror, dependencies = dependencies)
    }
  }
}
