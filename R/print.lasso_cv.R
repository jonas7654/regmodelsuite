#' Print function for the lasso_cv object
#' @param x a S3 lasso_cv object estimated by lasso_cv
#' @export

print.lasso_cv <- function(x) {
  cat("Lasso Cross-Validation Results\n")
  cat("==============================\n\n")

  cat("Number of lambda values tested:", x$nlambda, "\n")
  cat("Number of folds used:", x$m_folds, "\n\n")

  cat("Lambda values:\n")
  print(x$lambda_grid)

  cat("\n Mean Squared Prediction Error (MSPE):\n")
  print(min(x$MSPE))

  cat("\nOptimal Lambda:", x$min_lambda, "\n")

  invisible(x)
}
