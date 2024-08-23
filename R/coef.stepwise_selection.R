#' Extracts the coefficients of a stepwise selection object.
#'
#' @param object Stepwise selection object
#' @return The coefficients.
#'
#' @export
coef.stepwise_selection <- function(object) {
  stopifnot("object must be of type stepwise_selection" =
              inherits(object, "stepwise_selection"))

  betas <- object$model$coefficients

  if (any(is.na(betas))) {
    warning("Some coefficients are NA")
  }

  return(betas)
}
