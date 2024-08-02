#' Finds a subset of predictors using forward selection
#'
#' @param X Dataset X
#' @param Y Dataset y
#' @param n_predicors Amount of predictors to select
#' @param model Modeling function
#' @param verbose Whether to print information of the selection process
#'
#' @return A fitted model using the selected predictors.
#'
#' @return A double value
forward_selection <- function(X, y, n_predictors, model = lm, verbose = TRUE) {
  # Predictors not used in the latest model
  unused_predictors <- colnames(X)

  # Combining X and y for testing with lm (Remove later)
  X["y"] <- data.frame(y)

  # Formula used for the latest model
  formula_str <- "y ~"

  for(i in 1:n_predictors) {

    if(verbose)
      cat(sprintf("%dth predictor:\n", i))

    best_loss <- NULL

    # Iterating through the unused predictors
    for(p in seq_along(unused_predictors)) {

      # Adding a new predictor to the formula
      cur_formula_str <- paste(formula_str, unused_predictors[p])

      # Converting the current formula string to a formula object
      formula <- as.formula(cur_formula_str)

      # Fitting the model with the added predictor
      fit <- model(formula, X)

      # Predict the data with the fitted model
      prediction <- predict(fit, X)

      # Calculating squared loss
      loss <- sum((y - prediction) ^ 2) / length(prediction)

      if(verbose)
        cat(sprintf("  + %s: %f\n", unused_predictors[p], loss))

      # Checking if the loss is currently the best
      if(loss < best_loss || is.null(best_loss)) {
        best_loss <- loss
        best_new_predictor <- p
        best_fit <- fit
        best_formula_str <- cur_formula_str
      }
    }

    formula_str <- paste(best_formula_str, "+")

    if(verbose)
      cat(sprintf("Added predicor '%s', Loss: %f\n\n",
                 unused_predictors[best_new_predictor], best_loss))

    # Removing the used predictor from the unused predictors
    unused_predictors <- unused_predictors[-best_new_predictor]
  }

  best_fit
}
