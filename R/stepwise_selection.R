#' Finds a subset of predictors using forward selection
#'
#' @param X Dataset X
#' @param y Dataset y
#' @param n_predictors An integer which defines the amount of predictors to
#'   select.
#' @param model_fct A function which specifies the modelused.
#'   Needs to have the parameters \code{formula} and \code{data} and needs to
#'   return and object with a \code{predict} function implemented.
#' @param verbose A logical which specifies if the selection process should be
#'   printed.
#'
#' @return A stepwise_selection object
forward_selection <- function(X, y, n_predictors, model_fct = lm,
                               verbose = TRUE) {
  used_predictors <- c()
  errors <- c()
  iterations <- min(n_predictors, ncol(X))

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y), check.names = FALSE)

  # Changing the name of the new y column if there already was one
  colnames(X) <- c(colnames(X)[-ncol(X)], make.unique(colnames(X))[ncol(X)])

  formula_start <- paste(colnames(X)[ncol(X)], "~")

  unused_predictors <- colnames(X)[-ncol(X)]

  for(i in 1:iterations) {

    if(verbose)
      cat(sprintf("%dth iteration:\n", i))

    best_error <- NULL

    for(p in seq_along(unused_predictors)) {

      # Creating new combination of predictors to test
      cur_predictors <- c(used_predictors, unused_predictors[p])

      # Creating formula object with the new combination
      formula <- as.formula(paste(formula_start,
                                  paste("`", cur_predictors, "`",
                                        sep="", collapse="+")))

      fit <- model_fct(formula = formula, data = X)

      # Predicting the data with the fitted model
      prediction <- predict(fit)

      # Calculating squared error
      error <- sum((y - prediction) ^ 2) / length(prediction)

      if(verbose) {
        cat(sprintf("  + %s: %f\n", unused_predictors[p], error))
      }

      # Checking if the error is currently the best
      if(error < best_error || is.null(best_error)) {
        best_error <- error
        best_predictor <- p
        best_fit <- fit
      }
    }

    if(verbose) {
      cat(sprintf("Added predicor '%s', Error: %f\n\n",
                    unused_predictors[best_predictor], best_error))
    }

    errors <- c(errors, best_error)

    # Removing and adding the selected predictor from the lists
    used_predictors <- c(used_predictors, unused_predictors[best_predictor])
    unused_predictors <- unused_predictors[-best_predictor]
  }

  results <- list(model = best_fit,
                  predictors = used_predictors,
                  direction = "forward",
                  error = best_error,
                  errors = errors,
                  start_predictors = 1)

  class(results) <- "stepwise_selection"

  results
}

#' Finds a subset of predictors using backward selection
#'
#' @param X Dataset X
#' @param y Dataset y
#' @param n_predictors An integer which defines the amount of predictors to
#'   select.
#' @param model_fct A function which specifies the modelused.
#'   Needs to have the parameters \code{formula} and \code{data} and needs to
#'   return and object with a \code{predict} function implemented.
#' @param verbose A logical which specifies if the selection process should be
#'   printed.
#'
#' @return A stepwise_selection object
backward_selection <- function(X, y, n_predictors, model_fct = lm, verbose = TRUE) {
  stopifnot("More data points than predictors required of backward selection."
            = nrow(X) > ncol(X))

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y))

  formula_start <- paste(colnames(X)[ncol(X)], "~")

  used_predictors <- colnames(X)[-ncol(X)]
  errors <- c()
  iterations <- length(used_predictors) - n_predictors + 1

  stopifnot("Can not select that many predictors."
            = iterations > 1)

  for(i in 1:iterations) {

    best_error <- NULL

    # Creating formula object with the new combination
    formula <- as.formula(paste(formula_start,
                                  paste(used_predictors, collapse="+")))

    fit <- model_fct(formula = formula, data = X)

    # First error is irrelevant
    if(i != 1) {
      # Predicting the data with the fitted model
      prediction <- predict(fit)

      # Calculating squared error
      error <- sum((y - prediction) ^ 2) / length(prediction)

      errors <- c(errors, error)

      if(verbose)
        cat(sprintf("%f\n\n", error))
    }

    # The last iteration is just for fitting the model and calculating the error
    if(i == iterations)
      break

    if(verbose)
      cat(sprintf("%dth iteration:\n", i))

    # Calculating the Z-scores of the coefficients
    coef <- coef(summary(fit))
    scores <- coef[, "Estimate"][-1] / coef[, "Std. Error"][-1]

    if(verbose) {
      for(s in names(scores))
        cat(sprintf("  - %s Z-score: %f\n", s, scores[s]))
    }

    # Getting predictor with the lowest Z-score
    lowest_which <- which.min(abs(scores))
    lowest_name <- names(scores)[lowest_which]
    lowest_score <- scores[lowest_which]

    if(verbose)
      cat(sprintf("Removed predictor '%s' with Z-score: %f. Error: ",
                  lowest_name, lowest_score))

    # Removing the predictor with the lowest Z-score from the used predictors
    used_predictors <- used_predictors[-lowest_which]
  }

  results <- list(model = fit,
                  predictors = used_predictors,
                  error = error,
                  direction = "backward",
                  errors = errors,
                  start_predictors = ncol(X) - 2)

  class(results) <- "stepwise_selection"

  results
}
