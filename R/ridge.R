ridge <- function(X,y, lambda = 0){

  # standardize coefficients
  y_demeaned <- scale(y, scale = F)
  X_scaled <- scale(X)
  X_scaled_transposed <- t(X_scaled)

  ridge_coefficients <- solve(X_scaled_transposed %*% X_scaled + lambda * diag(ncol(X)),
                              X_scaled_transposed %*% y_demeaned)

  y_hat <- attr(y_demeaned, "scaled:center") + X_scaled %*% ridge_coefficients
  r2 <- calculate_R2(y , y_hat)

  result <-list(coefficients = as.vector(ridge_coefficients),
                lambda = lambda,
                y = y,
                R2 = r2,
                mean_y = attr(y_demeaned, "scaled:center"), # for rescaling when predicting
                mean_x = attr(X_scaled, "scaled:center"), # for rescaling when predicting
                sd_x = attr(X_scaled, "scaled:scale"), # for rescaling when predicting
                model = X,
                n = nrow(X),
                p = ncol(X))

  class(result) <- "ridge"

  return(result)
}





#test wie in Richter (am ende entfernen, beispiel von @example):
set.seed(21)
n <- 100
d <- 10
o <- 0.3
X <- matrix(runif(n * d), n, d) #unabhÃĪngig+gleichverteilt auf [0,1]
real_ridge <- runif(d, min = -1, max = 1) #die echten Werte fÃžr Beta
y <- X %*% real_ridge + rnorm(n, mean = 0, sd = o) #Y mit normalverteiltem Messfehler N(0,o^2)
test_ridge <- ridge(X,y,0)
as.vector(test_ridge)
real_ridge
