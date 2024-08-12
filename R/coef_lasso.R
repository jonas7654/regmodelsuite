coef.lasso <- function(lasso) {
  if (any(lasso$coefficients == 0)) {
    cat("Dropped variables:" , which(beta_est$coefficients == 0), "\n")
  }
  lasso$coefficients
}
