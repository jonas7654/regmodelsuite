#' Calculate model R-Squared
#'
#' @param y Response vector (observed values)
#' @param yhat Fitted values (predicted values)
#'
#' @return R-Squared statistic


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
