#' Extract coefficients from selection_model

coef.stepwise_selection <- function(object) {
  stopifnot("object must be of type stepwise_selection" =
              inherits(object, "stepwise_selection"))

  betas <- object$model$coefficients

  if (any(is.na(betas))) {
    warning("Some coefficients are NA")
  }

  return(betas)
}
