#' @export


predict.LAR <- function(object , newdata = NULL) {
  stopifnot("please provide a LAR object"  = inherits(object , "LAR"))


  beta <- object$coefficients[nrow(object$coefficients) , ]
  mean_y <- object$mean_y
  means_X <- object$mean_x  # Mean of training data used for standardization
  sds_X <- object$sd_x      # Standard deviation of training data used for standardization


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
    X <- object$model # model is scaled
  }


  # Return predictions
  y_hat <- mean_y + X %*% beta
  y_hat <- as.vector(y_hat)

  return(y_hat)
}
