#' Calculate the LARS path
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#'
#' @return list ..... TODO


### TODO ####
# plot(...) for plotting L1-arc length
# step output
# Lasso option !!!!
# How to select the "best" model? CV? or just provide all models an the user can decide ?
# Include stepsize alpha !!!!!!!!!


least_angle_regression <- function(X, y, iter = NULL, verbose = T) {
  # Initialize
  n <- nrow(X)
  p <- ncol(X)

  if (is.null(iter)) {
    # We arrive at the least squares solution after min(n-1, p) steps
    iter <- min(n - 1, p)
  }

  # Standardize regressors and initialize the first residuum
  X_scaled <- scale(X)
  # pre calculate X_scaled for greater efficiency
  X_scaled_transposed <- t(X_scaled)
  Xt_X_prod_inverse <- solve(X_scaled_transposed %*% X_scaled)

  r <- y - mean(y)

  coefficient_matrix <- matrix(NA, nrow = iter, ncol = p)


  # Helper
  active_variables <- logical(p)
  beta <- double(p)

  for (i in 1:iter) {
    # Find the regressor x most correlated with current r (not in)
    r_cor <- cor(X_scaled, r) # use t(X) %*% r ?
    x_index <- which.max(abs(r_cor))

    # update the active set (drop = FALSE in order to keep the class of X)
    active_variables[x_index] <- TRUE
    A <- X_scaled[, active_variables, drop = FALSE]

    # stepsize alpha
    # B is X_tilde

    B <- sign(r_cor[x_index]) * A
    B_transpose <- t(B)
    Bt_B_prod_inverse <- solve(B_transpose %*% B)
    ones <- matrix(1, ncol(A), 1)
    ones_tranpose <- t(ones)

    # calculate w
    w <- ones_tranpose %*% Bt_B_prod_inverse %*% ones
    sqrt_w <- as.double(sqrt(w))

    u <- ((B %*% Bt_B_prod_inverse) %*% ones) / sqrt_w

    C_max <- r_cor[x_index]
    C_j <- r_cor[-active_variables]

    # Take the min
    alpha_pos <- (C_max - C_j) / ((1 / sqrt_w) - t(X_scaled[, -active_variables]) %*% u)
    alpha_neg <- (C_max + C_j) / ((1 / sqrt_w) + t(X_scaled[, -active_variables]) %*% u)
    alpha <- min(c(alpha_pos, alpha_neg)[c(alpha_pos, alpha_neg) > 0])

    # Calculate the current model and update beta
    delta_step <- solve(t(A) %*% A, t(A) %*% r)
    beta[active_variables] <- beta[active_variables] + alpha * delta_step

    # update the residual
    r <- y - A %*% as.matrix(beta[active_variables])
    #r <- r - A %*% delta_step

    ############################################################################

    # Verbose option
    if (verbose) {
      cat("Iteration:", i, "\n")
      cat("Active Variables:", which(active_variables), "\n")
      cat("delta :", delta_step, "\n")
      cat("alpha :", alpha, "\n")
      cat("Coefficients:", beta, "\n")
      cat("\n")
    }

    # update output data
    coefficient_matrix[i, ] <- beta
  }

  # Calculate arc length
  arg_length <- apply(coefficient_matrix, 1, function(x) {sum(abs(x))})

  # Calculate R-Squared for each model
  r2 <- apply(coefficient_matrix, 1, function(beta) {calculate_R2(y, X %*% beta)}
             )

  output_list <- list(coefficients = coefficient_matrix,
                      l1_arc_length = arg_length,
                      R2 = r2)

  # Modify S3 class
  class(output_list) <- "LAR"

  return (output_list)

}
