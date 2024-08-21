#' Predict function for stepwise_selection()

predict.stepwise_selection <- function(object, newdata = NULL) {
  stopifnot("object must be of type stepwise_selection" =
              inherits(object,stepwise_selection))


}
