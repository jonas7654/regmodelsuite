#' @export


predict.lasso <- function(object , newdata = NULL) {
  stopifnot("please provide a lasso object"  = inherits(object , "lasso"))

  beta <- object$coefficients
  mean_y <- object$y_mean
  means_X <- object$x_mean  # Mean of training data used for standardization
  sds_X <- object$x_sd   # Standard deviation of training data used for standardization


  # Check if newdata is provided
  if (!is.null(newdata)) {

    stopifnot("newdata must be a dataframe" = is.data.frame(newdata))


    # Ensure newdata has the correct dimensions
    stopifnot("wrong dimensions" = (ncol(newdata) == ncol(object$model)))

    # standardize newdata
    newdata_scaled <- scale(newdata, center = means_X, scale = sds_X)

    X <- as.matrix(newdata_scaled)
  } else {
    # Use the original model data for prediction
    X <- object$model
  }


  # Return predictions
  y_hat <- mean_y + X %*% beta

  return(y_hat)
}
