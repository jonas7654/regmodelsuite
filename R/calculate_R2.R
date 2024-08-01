#' Calculate model R-Squared
#'
#' @param y response vector
#' @param yhat fitted values
#'
#' @return R-Squared



calculate_R2 <- function(y, yhat) {
  mean_y <- mean(y)

  r2 <- 1 - sum((y - yhat)^2) / sum((y-mean_y)^2)
  return(r2)
}
