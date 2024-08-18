coef.lasso <- function(object) {
  stopifnot("object must be of type lasso" = inherits(object, "lasso"))

  betas <- object$coefficient

  return(betas)
}
