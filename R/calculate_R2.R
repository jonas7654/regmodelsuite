#' Calculate model R-Squared
#'
#' @param y Response vector (observed values)
#' @param yhat Fitted values (predicted values)
#'
#' @return R-Squared statistic
#'
#' @examples
#' y <- c(3, 5, 2, 8, 7)
#' yhat <- c(2.8, 5.1, 2.4, 7.8, 7.2)
#' calculate_R2(y, yhat)
calculate_R2 <- function(y, yhat) {
  # Ensure y and yhat have the same length
  if (length(y) != length(yhat)) {
    stop("Length of y and yhat must be the same")
  }

  # Check for non-NA values
  if (any(is.na(y)) || any(is.na(yhat))) {
    stop("y and yhat should not contain NA values")
  }

  mean_y <- mean(y)

  r2 <- 1 - sum((y - yhat)^2) / sum((y-mean_y)^2)
  return(r2)
}
