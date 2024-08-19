#' Extract coefficients from an estimated least angle regression model
#'
#' @param object A LAR S3 Object
#'
#' @return LAR coefficients from regobject
#'
#' @export



coef.lasso <- function(object) {
  stopifnot("object must be of type lasso" = inherits(object, "lasso"))

  betas <- object$coefficient

  return(betas)
}
