#' Prints information of a stepwise selection object.
#'
#' @param object Stepwise selection object
#'
#' @export
print.stepwise_selection <- function(object) {
  cat("Stepwise selection object\n")
  cat("-------------------------\n")
  cat(sprintf("Selected predictors: %s\n",
              paste(object$predictors, collapse=", ")))
  cat(sprintf("Error: %f\n", object$error))
  cat(sprintf("Direction: %s", object$direction))
}
