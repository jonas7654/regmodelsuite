#' Extract coefficients from an estimated least angle regression model
#'
#' @param object A LAR S3 Object
#' @param unscale If TRUE, the coefficients will be scaled back to the original
#'   scale
#' @return LAR coefficients from regobject
#'
#' @export



coef.lasso <- function(object, unscale = FALSE) {
  stopifnot("object must be of type lasso" = inherits(object, "lasso"))

  betas <- object$coefficient

  if (unscale) {
    betas <- betas / object$sd_x
  }

  return(betas)
}
