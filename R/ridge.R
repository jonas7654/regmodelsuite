ridge <- function(X,y, lambda = 0){

  # standardize coefficients
  y_demeaned <- scale(y, scale = F)
  X_scaled <- scale(X)
  X_scaled_transposed <- t(X_scaled)

  ridge_coefficients <- solve(X_scaled_transposed %*% X_scaled + lambda * diag(ncol(X)),
                              X_scaled_transposed %*% y_demeaned)
  stopifnot("couldn't calculate coefficients. Data is probably poor conditioned"
            = !is.nan(ridge_coefficients))

  y_hat <- attr(y_demeaned, "scaled:center") + X_scaled %*% ridge_coefficients
  r2 <- calculate_R2(y , y_hat)

  result <-list(coefficients = as.vector(ridge_coefficients),
                lambda = lambda,
                y = y,
                R2 = r2,
                mean_y = attr(y_demeaned, "scaled:center"), # for rescaling when predicting
                mean_x = attr(X_scaled, "scaled:center"), # for rescaling when predicting
                sd_x = attr(X_scaled, "scaled:scale"), # for rescaling when predicting
                model = X_scaled,
                n = nrow(X),
                p = ncol(X))

  class(result) <- "ridge"

  return(result)
}


