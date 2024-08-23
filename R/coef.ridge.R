#' Extract coefficients from an estimated ridge model
#'
#' @param object A ridge S3 Object
#' @param unscale If TRUE, the coefficients will be scaled back to the original
#'   scale
#'
#' @return ridge coefficients from regobject
#'
#' @export


coef.ridge <- function(object, unscale = FALSE) {
  stopifnot("object must be of type ridge" = inherits(object, "ridge"))

  betas <- object$coefficient

  if (unscale) {
    betas <- betas / object$sd_x
  }

  return(betas)
}
