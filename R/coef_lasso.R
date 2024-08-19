#' Extract coefficients from an estimated lasso model
#'
#' @param lasso A lasso S3 Object
#'
#' @return lasso coefficients from regobject
#'
#' @export

coef.lasso <- function(regobject) {

  extracted_model<- lapply(regobject, function(x) {
    beta <- x$coefficients
  })

  if (any(lasso$coefficients == 0)) {
    cat("Dropped variables:" , which(beta_est$coefficients == 0), "\n")
  }
  lasso$coefficients
}
