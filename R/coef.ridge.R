#' Extract coefficients from an estimated ridge model
#'
#' @param object A ridge S3 Object
#'
#' @return ridge coefficients from regobject
#'
#' @export


coef.ridge <- function(object) {
  stopifnot("object must be of type ridge" = inherits(object, "ridge"))

  betas <- object$coefficient

  return(betas)
}
