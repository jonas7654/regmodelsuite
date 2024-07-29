#' Calculate the LARS path
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#'
#' @return list ..... TODO


### TODO ####
# Verbose option
# plot(...) for plotting L1-arc length
# step output
# Lasso option


least_angle_regression <- function(X, y, iter = 10, verbose = FALSE) {

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
    delta_step <- solve(t(A) %*% A, t(A) %*% r) # .Internal(La_solve(t(A) %*% A, t(A) %*% r, .Machine$double.eps))
    beta[active_variables] <- beta[active_variables] + delta_step

    # update the residual
    r <- y - A %*% as.matrix(beta[active_variables])

    # Verbose option : TODO
    if(verbose) {
      active_set_verbose <- as.array(active_variables)
      dimnames(active_set_verbose) <- colnames(X)[active_variables]
    }
    # update output data
    coefficient_matrix[i, ] <- beta
  }

  # Calculate arc length
  arg_length <- apply(coefficient_matrix, 1, function(x) {sum(abs(x))})

  output_list <- list(coefficients = coefficient_matrix,
                      l1_arc_length = arg_length)
  class(output_list) <- "LAR"

  return (output_list)

}
