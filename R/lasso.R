#' Calculate the Lasso estimates
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#' @param lambda penalty parameter ($\lambda > 0$)
#' @param tol tolerance level for convergence (default = 1e-5)
#'
#' @return list ..... TODO


lasso <- function(X, y, lambda,  tol = 1e-07, verbose = F) {

  if (lambda == 0) {
    OLS <- lm.fit(X, y)$coefficients # solve(t(X) %*% X, t(X) %*% y)
    if (ncol(X) >= nrow(X)) {
      warning("The matrix X suffers from multicollinearity")
    }
    return(list(coefficients = OLS))
  }


  n <- nrow(X)
  p <- ncol(X)


  # Init
  X_scaled <- scale(X, scale = F)
  beta <- double(p)
  max_abs_beta_diff <- Inf

  mean_X_scaled_squared <- colMeans(X_scaled^2)

  m <- 0L


  while(max_abs_beta_diff > tol) {

    # Only for debugging
    if(verbose) {
      cat("Iteration:", m, "\n")
      cat("beta_diff:", max_abs_beta_diff, "\n")
    }


    beta_old <- beta

    for (j in 1:p) {
      # 1
      r <- y - X_scaled[ , -j] %*% beta[-j]

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

  # Output section


  lasso_obj <- list(coefficients = round(beta, 6),
                    iterations = m,
                    lambda = lambda)
  class(lasso_obj) <- "lasso"

  return(lasso_obj)

}
