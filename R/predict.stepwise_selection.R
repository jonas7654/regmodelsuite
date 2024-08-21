#' Predict function for stepwise_selection()

predict.stepwise_selection <- function(selectionobject, newdata = NULL) {
  stopifnot("object must be of type stepwise_selection" =
              inherits(selectionobject,"stepwise_selection"))

  object <- selectionobject$model

  y_hat <- predict(object , newdata)

  return(y_hat)
}
