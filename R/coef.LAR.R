coef.LAR <- function(object) {
    stopifnot("object must be of type LAR" = inherits(object, "LAR"))

    betas <- object$coefficient

    return(betas)
}
