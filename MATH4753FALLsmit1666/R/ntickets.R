#' Title
#'
#' @importFrom stats dnorm pbinom pnorm
#'
#' @param N the minimun number of tickets to be sold
#' @param gamma the probability that the flight is overbooked
#' @param p the probability that a passenger shows up
#'
#' @return a named list containing nd, nc, N, p and gamma and two plots, one of the discrete value and one of the continuous approximation
#' @export
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  #graphics.off()
  n <- seq(N, floor(N + N/10), by = 1)
  tmp <- 1 - gamma - pbinom(q = N, size = n, prob = p)

  ind <- which.min(abs(tmp))

  df <- data.frame(n, tmp)

  n2 <- seq(N, floor(N + N/10), by = 0.0001)

  tmp2 <- 1 - gamma - pnorm(q = N + 1/2, mean = n2*p, sd = sqrt(n2*p*(1-p)))

  ind2 <- which.min(abs(tmp2))

  df2 <- data.frame(n2, tmp2)

  nd <- n[tmp==tmp[ind]]

  nc <- n2[tmp2==tmp2[ind2]]

  list_items <- c("nd", "nc", "N", "p", "gamma")

  list_names <- c(nd, nc, N, p, gamma)

  named_list <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  print(named_list)

  layout(matrix(1:2, nrow = 2, ncol = 1))

  plot(df, type = "b", ylab = "Objective", pch = 16, cex = 0.75, col = ifelse(tmp == tmp[ind], "red", "black"), main = paste("Objective Vs n to find optimal tickets sold", "\n", "(", nd, ") gamma = ", gamma, " N = ", N,  "discrete"))
  abline(h = tmp[ind], v= n[ind], col = "red")

  plot(df2, type = "l", xlab = "n", ylab = "Objective", main = paste("Objective Vs n to find optimal tickets sold", "\n", "(", nc, ") gamma = ", gamma, " N = ", N,  "continuous"))
  abline(h = tmp2[ind2], v= n2[ind2], col = "blue")

}

