#' Predicts data using a stepwise selection object.
#'
#' @param object Stepwise selection object
#' @param newdata Data to predict, if left empty predicts the data the model
#' was fitted on.
#' @return The predicted data.
#'
#' @export
predict.stepwise_selection <- function(object, newdata = NULL) {
  stopifnot("object must be of type stepwise_selection" =
              inherits(object,"stepwise_selection"))

  y_hat <- predict(object$model , newdata)

  return(y_hat)
}
