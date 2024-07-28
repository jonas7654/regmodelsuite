#' Calculate the LARS path
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#'
#' @return list ..... TODO



least_angle_regression <- function(X, y, iter = 10) {

  n <- nrow(X)
  p <- ncol(X)
  coefficient_matrix <- matrix(NA, iter, p)

  # Standardize regressors and initialize the first residuum
  X_scaled <- scale(X)
  r <- y - mean(y)

  # Helper
  active_variables <- logical(p)
  beta <- double(p)

  for (i in 1:iter) {
    # Find the regressor x most correlated with current r
    r_cor <- cor(X_scaled[, !active_variables], r)
    x_index <- which.max(abs(r_cor))

    # update the active set
    active_variables[x_index] <- TRUE
    A <- X_scaled[, active_variables]

    # Calculate the current model and update beta
    delta_step <- solve(t(A) %*% A, t(A) %*% r)
    beta[active_variables] <- beta[active_variables] + delta_step

    # update the residual
    r <- y - A %*% as.matrix(beta[active_variables])

    # update output data
    coefficient_matrix[i, ] <- beta
    print(as.integer(active_variables))
  }




  return (list(coefficient_matrix))

}
