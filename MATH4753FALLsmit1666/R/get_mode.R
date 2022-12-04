#' Title
#'
#' @param v A quantitative vector of data
#'
#' @return the mode of the given quantitative vector of data
#' @export
#'
#' @examples
#'getmode(c(1, 1, 2 , 3))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
