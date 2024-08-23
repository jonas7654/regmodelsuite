lasso <- function(X, y, lambda,  tol = 1e-07, verbose = F) {

  X_scaled <- scale(X, scale = T)
  y_demeaned <- scale(y , scale = F)


  if (lambda == 0) {
    beta <- lm.fit(X_scaled, y_demeaned)$coefficients # solve(t(X) %*% X, t(X) %*% y)
    if (ncol(X) >= nrow(X)) {
      warning("The matrix X suffers from multicollinearity")
    }

    # Output section
    y_hat <- attr(y_demeaned, "scaled:center") + X_scaled %*% beta
    r2 <- calculate_R2(y , y_hat)
    dim(r2) <- c(length(r2), 1)


    lasso_obj <- list(coefficients = beta,
                      lambda = lambda,
                      R2 = r2,
                      y = y,
                      mean_y = attr(y_demeaned, "scaled:center"),
                      mean_x = attr(X_scaled, "scaled:center"),
                      sd_x = attr(X_scaled, "scaled:scale"),
                      model = X_scaled,
                      n = n
    )

  }


  n <- nrow(X)
  p <- ncol(X)


  # Init

  beta <- double(p)
  max_abs_beta_diff <- Inf

  mean_X_scaled_squared <- colMeans(X_scaled^2)

  m <- 1L

  max_iterations <- 1000


  while((max_abs_beta_diff > tol) && (m <= max_iterations)) {



    # Only for debugging
    if(verbose) {
      cat("Iteration:", m, "\n")
      cat("beta_diff:", max_abs_beta_diff, "\n")
    }


    beta_old <- beta

    for (j in 1:p) {

      # 1
      r <- y_demeaned - X_scaled[ , -j, drop = FALSE] %*% beta[-j]

      # 2
      beta_j_tilde <- mean(X_scaled[ , j] * r)

      # 3
      beta_j_next <- ifelse(j == 1, beta_j_tilde,
                            (1 / mean_X_scaled_squared[j]) *
                              sign(beta_j_tilde) *
                              max(0, (abs(beta_j_tilde) - (lambda/2)))
                           )
      beta[j] <- beta_j_next

    }

    max_abs_beta_diff <- max(abs(beta - beta_old))
    m <- m + 1

  }

  stopifnot("couldn't calculate coefficients. Data is probably poor conditioned"
            = !is.nan(beta))


  # Output section
  y_hat <- attr(y_demeaned, "scaled:center") + X_scaled %*% beta
  r2 <- calculate_R2(y , y_hat)
  dim(r2) <- c(length(r2), 1)


  lasso_obj <- list(coefficients = beta,
                    active_variables = NA,
                    inactive_variables = NA,
                    iterations = m,
                    lambda = lambda,
                    R2 = r2,
                    y = y,
                    mean_y = attr(y_demeaned, "scaled:center"),
                    mean_x = attr(X_scaled, "scaled:center"),
                    sd_x = attr(X_scaled, "scaled:scale"),
                    model = X_scaled,
                    n = n
                    )

  class(lasso_obj) <- "lasso"

  return(lasso_obj)

}
