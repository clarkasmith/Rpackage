#' Title
#'
#' @importFrom graphics abline barplot curve layout polygon
#'
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distribution
#' @param a the upper limit of the sequence of which the area is being found
#'
#' @return a plot of the normal distribution given the parameters
#' @export
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x, mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve <- seq(mu-3*sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "Red")

  prob <- list(pnorm(a, mu, sigma))
  prob
}
