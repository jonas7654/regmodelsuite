#' Extract coefficients from an estimated least angle regression model
#'
#' @param object A LAR S3 Object
#' @param unscale If TRUE, the coefficients will be scaled back to the original
#'   scale
#'
#' @return LAR coefficients from regobject
#'
#' @export



coef.LAR <- function(object, unscale = T) {
    stopifnot("object must be of type LAR" = inherits(object, "LAR"))

    betas <- object$coefficient[nrow(object$coefficients) , ]

    if (unscale) {
      betas <- betas / object$sd_x
    }

    return(betas)
}
