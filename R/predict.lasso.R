#' @export


predict.lasso <- function(object , newdata = NULL) {
  stopifnot("please provide a lasso object"  = inherits(object , "lasso"))

  beta <- object$coefficients
  mean_y <- object$mean_y
  means_X <- object$mean_x  # Mean of training data used for standardization
  sds_X <- object$sd_x   # Standard deviation of training data used for standardization
  model_formula <- object$formula

  # Check if newdata is provided
  if (!is.null(newdata)) {

    stopifnot("newdata must be a dataframe" = is.data.frame(newdata))

    new_data_model_matrix <- model.matrix(model_formula, data = newdata)

    # Ensure newdata has the correct dimensions
    stopifnot("wrong dimensions" = (ncol(new_data_model_matrix) == ncol(object$model)))

    # standardize newdata
    newdata_scaled <- scale(new_data_model_matrix,
                            center = means_X,
                            scale = sds_X)

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
