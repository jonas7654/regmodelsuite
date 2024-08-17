ridge_plots <- function(X,y,m){
  ridge_plot_profile_lines(X,y,m)
  ridge_plot(X,y,m)
}

ridge_plot <- function(X,y,m){
  cv_line <- X %*% ridge(X,y,ridge_cv(X,y,m)$cv.lambda)
  plot(X[,1], y, main="Ridge-Schätzer", xlab="x1", ylab="y", pch=19, col="blue")
  lines(sort(X[,1]), cv_line, col="red", lwd=2)
}

ridge_plot_profile_lines <- function(X,y,m){
  lambda_seq <- 10^seq(3, -3, by = -0.1)
  ridge_seq <- matrix(0, nrow = ncol(X), ncol = length(lambda_seq))
  for (i in seq_along(lambda_seq)) {
    lambda <- lambda_seq[i]
    ridge_seq[, i] <- ridge(X, y, lambda)
  }

  matplot(lambda_seq, t(ridge_seq),
          type = "l", lty = 1, col = 1:ncol(ridge_seq),
          xlab = "lambda", ylab = "geschätzte Parameter beta",
          main = "Ridge-Schätzer: Profillinien der Parameter", log = "x", xaxt = "n"
  )

  axis(1, at = c(1e-3, 1e-1, 1e1, 1e3), labels = c("1e-3", "1e-1", "1e1", "1e3"))

  abline(v = cross_validation(X,y,m)$cv.lambda, col = "black", lty = 2)
}
