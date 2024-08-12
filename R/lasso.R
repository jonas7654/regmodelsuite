#' Calculate the Lasso estimates
#'
#' @param X data frame containing the covariates
#' @param Y response vector
#'
#' @return list ..... TODO


lasso <- function(X, y, lambda,  tol = 1e05) {

  n <- nrow(X)
  p <- ncol(X)
  gamma <- tol

  # Init
  X_scaled <- scale(X, center = F)
  beta <- double(p)
  max_abs_beta_diff <- Inf
  m <- 0L


  while(gamma < max_abs_beta_diff) {

    beta_old <- beta

    for (j in 1:p) {
      # 1
      r <- y - X_scaled[ , -j] %*% beta[-j]

      # 2
      beta_j_tilde <- mean(X_scaled[ , j] * r)

      # 3
      beta_j_next <- ifelse(j == 0, beta_j_tilde,
                            (1 / (mean(X_scaled[ , j]^2))) *
                              sign(beta_j_tilde) *
                              (abs(beta_j_tilde) - (lambda/2))
                           )
      beta[j] <- beta_j_next

      # 4
      m <- m + 1 # unnötig
    }

    max_abs_beta_diff <- max(abs(beta - beta_old))

  }

  return(beta)

}
