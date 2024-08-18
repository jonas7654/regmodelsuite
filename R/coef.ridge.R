coef.ridge <- function(object) {
  stopifnot("object must be of type ridge" = inherits(object, "ridge"))

  betas <- object$coefficient

  return(betas)
}
