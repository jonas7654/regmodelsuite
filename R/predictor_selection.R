#' Finds a subset of predictors using forward or backward selection
#'
#' @param X Dataset X
#' @param Y Dataset y
#' @param n_predicors Amount of predictors to select
#' @param model Modeling function
#' @param direction Selection direction, either "forward" or "backward"
#' @param verbose Whether to print information of the selection process
#'
#' @return A fitted model using the selected predictors.
#'
#' @return A double value
select <- function(X, y, n_predictors, model = lm, direction = "forward", verbose = TRUE) {

  forward <- FALSE

  # Initializing the variables for the selected direction
  if(direction == "forward") {
    unused_predictors <- colnames(X)
    used_predictors <- c()
    iterations <- n_predictors
    forward <- TRUE
  }
  else if(direction == "backward") {
    unused_predictors <- c()
    used_predictors <- colnames(X)
    iterations <- length(used_predictors) - n_predictors
  }
  else
    stop("direction has to be \"forward\" or \"backward\".")

  # Combining X and y for testing with lm (Remove later)
  X["y"] <- data.frame(y)

  for(i in 1:iterations) {

    if(verbose)
      cat(sprintf("%dth predictor:\n", i))

    best_loss <- NULL

    # Iterating through the predictors not added or removed
    for(p in seq_along(if(forward) unused_predictors else used_predictors)) {

      # Creating new combination of predictors to test
      if(forward)
        cur_predictors <- c(used_predictors, unused_predictors[p])
      else
        cur_predictors <- used_predictors[-p]

      # Creating formula object with the new combination
      formula <- as.formula(paste("y ~", paste(cur_predictors, collapse="+")))

      fit <- model(formula, X)

      # Predicting the data with the fitted model
      prediction <- predict(fit, X)

      # Calculating squared loss
      loss <- sum((y - prediction) ^ 2) / length(prediction)

      if(verbose) {
        if(forward)
          cat(sprintf("  + %s: %f\n", unused_predictors[p], loss))
        else
          cat(sprintf("  - %s: %f\n", used_predictors[p], loss))
      }

      # Checking if the loss is currently the best
      if(loss < best_loss || is.null(best_loss)) {
        best_loss <- loss
        best_predictor <- p
        best_fit <- fit
      }
    }

    if(verbose) {
      if(forward)
        cat(sprintf("Added predicor '%s', Loss: %f\n\n",
                    unused_predictors[best_predictor], best_loss))
      else
        cat(sprintf("Removed predicor '%s', Loss: %f\n\n",
                    used_predictors[best_predictor], best_loss))
    }

    # Removing and adding the selected predictor from the lists
    if(forward) {
      used_predictors <- c(used_predictors, unused_predictors[best_predictor])
      unused_predictors <- unused_predictors[-best_predictor]
    }
    else {
      unused_predictors <- c(unused_predictors, used_predictors[best_predictor])
      used_predictors <- used_predictors[-best_predictor]
    }
  }

  best_fit
}
