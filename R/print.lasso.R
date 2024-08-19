print.lasso <- function(x, ...) {
  cat("Lasso Regression Results\n")
  cat("========================\n\n")

  cat("Lambda:", x$lambda, "\n")
  cat("Number of iterations:", x$iterations, "\n\n")

  cat("Coefficients:\n")
  print(x$coefficients)

  cat("\nActive Variables:\n")
  if (length(x$active_variables) > 0) {
    cat(paste(x$active_variables, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }

  cat("\nInactive Variables:\n")
  if (length(x$inactive_variables) > 0) {
    cat(paste(x$inactive_variables, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }

  invisible(x)
}
