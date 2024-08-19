#' Extract coefficients from an estimated least angle regression model
#'
#' @param object A LAR S3 Object
#'
#' @return LAR coefficients from regobject
#'
#' @export



coef.LAR <- function(object) {
    stopifnot("object must be of type LAR" = inherits(object, "LAR"))

    betas <- object$coefficient

    return(betas)
}
