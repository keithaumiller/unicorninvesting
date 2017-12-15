INSTALLED_PACKAGES <- rownames(installed.packages())

#' is.installed
#'
#' Checks whether a package has been installed.
#' @param string package name
#' @return logical TRUE if the package has been installed, else FALSE.
#' @examples
#' is.installed('ggplot2')
is.installed   <- function (package) {
 	evaluation <- package %in% INSTALLED_PACKAGES

 	return(evaluation)
}

is.equal       <- function (x, y) {
 	evaluation <- x == y

 	return(evaluation)
}

is.true        <- function (x) {
 	evaluation <- is.equal(x, TRUE)

 	return(evaluation)
}

is.empty       <- function (x) {
	if ( is.data.frame(x) ) {
		length <- nrow(x)
	} else {
		length <- length(x)
	}

	evaluation <- is.equal(length, 0)

 	return(evaluation)
}
