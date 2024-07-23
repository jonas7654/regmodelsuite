
least_angle_regression <- function(X, y, iter = 1000) {

  # standardize regressors
  X_scaled <- scale(X)

  n <- nrow(X)
  p <- ncol(X)

  # Initialize values
  beta <- rep(0, p)
  r <- y - mean(y)  # residua
  active_set <- logical(p)
  x_candidates <- 1:p

  for (i in 1:iter) {

    # calculate the correlation between r and x_j
    r_x_cor <- cor(r, X_scaled)

    # Get index of x_i which has the highest correlation
    x_index <- which.max(abs(r_x_cor))

    # Update the active set
    active_set[x_index] <- TRUE
    x_candidates <- setdiff(x_candidates, x_index)


    A <- X[, active_set]

    # Calculate delta and u
    delta = solve(t(A) %*% A, t(A) %*% r)
    u <- A %*% delta

    # Move betas
    beta[x_index] <- beta[x_index] + alpha * delta


    # TODO


    # TODO


    # update residual
    r <- y - X_scaled %*% beta
  }

  return (list(delta, beta, u , r))

}
