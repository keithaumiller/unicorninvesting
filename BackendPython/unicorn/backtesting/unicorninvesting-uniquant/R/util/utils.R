source('util/log.R')
source('util/is.R')
source('util/install.R')
source('util/assign.R')

#' join
#'
#' Concatenates a list of strings seperated by a defined seperator.
#' @param list vector of strings
#' @param seperator, defaults to ''
#' @examples
#' join(c('a', 'b'), seperator = ', ')
#' [1] "a, b"
join       <- function (list, seperator = '') {
	string <- paste(list, collapse = seperator)

  	return(string)
}