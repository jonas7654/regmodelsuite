#' Calculate the LARS path
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#'
#' @return list ..... TODO


### TODO ####
# plot(...) for plotting L1-arc length



least_angle_regression <- function(X, y, verbose = F) {

  n <- nrow(X)
  p <- ncol(X)
  max_iter <- min(n - 1, p)

  # scale variables
  X_scaled <- scale(X)
  y_demeaned <- scale(y, scale = F)

  # Init
  r <- y_demeaned
  beta <- double(p)
  active_variables <- logical(p)
  coefficient_matrix <- matrix(0, max_iter, p)

  for (i in 1:max_iter) {
    # Calculate correlation
    C_j <- t(X) %*% r

    j_star <- which.max(abs((C_j[!active_variables , ])))
    C_max <- C_j[j_star]

    active_variables[j_star] <- TRUE


    A <- X_scaled[ , active_variables, drop = FALSE]

    #
    A_tilde <- A * sign(C_j[active_variables , ])

    A_tilde_A_inverse <- solve(t(A_tilde) %*% A_tilde)
    ones <- matrix(1, nrow(A_tilde_A_inverse), 1)
    ones_transposed <- t(ones)


    w <- ones_transposed %*% A_tilde_A_inverse %*% ones
    sqrt_w <- as.double(sqrt(w))

    u <- (A_tilde %*% A_tilde_A_inverse %*% matrix(1, ncol(A_tilde %*% A_tilde_A_inverse) , 1)) / sqrt_w

    # calculate step size
    B <- t(X_scaled) %*% u

    if (i == max_iter) {
      alpha <- C_max * sqrt_w
    }
    else {
      alpha_neg <- (C_max - C_j[!active_variables]) / ((1 / sqrt_w) - B[!active_variables, ])
      alpha_pos <- (C_max + C_j[!active_variables]) / ((1 / sqrt_w) + B[!active_variables, ])
      alpha <- min(c(alpha_neg[alpha_neg >= 0], alpha_pos[alpha_pos >= 0], 0), na.rm = TRUE)
    }


    # update beta and r
    delta_step <- sign(C_j[active_variables, ]) * solve(t(A) %*% A, t(A) %*% u)

    beta[active_variables] <- beta[active_variables] + alpha * delta_step

    r <- r - alpha * u
    #r <- r - A %*% delta_step

    ############################################################################

    # Verbose option only for debugging
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
  l1_arc_length <- rowSums(abs(coefficient_matrix))

  # Calculate R-Squared for each model
  r2 <- apply(coefficient_matrix, 1, function(beta) {calculate_R2(y, X %*% beta)}
             )
  dim(r2) <- c(length(r2), 1)

  output_list <- list(coefficients = coefficient_matrix,
                      l1_arc_length = l1_arc_length,
                      R2 = r2,
                      y = y,
                      mean_y = attr(y_demeaned, "scaled:center"),
                      mean_x = attr(X_scaled, "scaled:center"),
                      sd_x = attr(X_scaled, "scaled:scale"),
                      model = X_scaled,
                      y = y,
                      n = n,
                      p = p)

  # Modify S3 class
  class(output_list) <- "LAR"

  return (output_list)



}
