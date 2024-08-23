#' Extract coefficients created during the cross validation
#'
#' @param object A cv S3 Object
#' @param unscale If TRUE, the coefficients will be scaled back to the original
#'   scale
#'
#' @return ridge or lasso coefficients from regobject, depending on which one
#'    was used for the cross validations.
#'
#' @export


coef.cv <- function(object, unscale = FALSE) {
  stopifnot("object must be of type cv" = inherits(object, "cv"))

  betas <- object$all_coefficient

  means <- colMeans(object$X)

  for (i in 1:ncol(betas)) {
    if (unscale) {
      betas[,i] <- betas[,i] <- betas[,i] / means
    }
  }

  return(betas)
}
