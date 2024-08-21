#' Finds a subset of predictors using forward selection
#'
#' @param X Dataset X
#' @param y Dataset y
#' @param n_predictors Amount of predictors to select
#' @param model_fct Modeling function with formula and data parameter (like lm)
#' @param verbose Whether to print information of the selection process
#'
#' @return A stepwise_selection object
forward_selection <- function(X, y, n_predictors, model_fct = lm,
                               verbose = TRUE) {

  stopifnot("n_predictors has to be bigger than 0." = n_predictors > 0)

  used_predictors <- c()
  errors <- c()
  iterations <- min(n_predictors, ncol(X))

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y))

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
                                  paste(cur_predictors, collapse="+")))

      fit <- model_fct(formula = formula, data = X)

      # Predicting the data with the fitted model
      prediction <- predict(fit, X)

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
#' @param n_predicors Amount of predictors to select
#' @param model_fct Modeling function with formula and data parameter (like lm)
#' @param verbose Whether to print information of the selection process
#'
#' @return A stepwise_selection object
backward_selection <- function(X, y, n_predictors, model_fct = lm, verbose = TRUE) {

  stopifnot("n_predictors has to be bigger than 0." = n_predictors > 0)

  stopifnot("More data points than predictors required."
            = nrow(X) > ncol(X))

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y))

  formula_start <- paste(colnames(X)[ncol(X)], "~")

  used_predictors <- colnames(X)[-ncol(X)]
  errors <- c()
  iterations <- length(used_predictors) - n_predictors + 1

  for(i in 1:iterations) {

    best_error <- NULL

    # Creating formula object with the new combination
    formula <- as.formula(paste(formula_start,
                                  paste(used_predictors, collapse="+")))

    fit <- model_fct(formula = formula, data = X)

    # First error is irrelevant
    if(i != 1) {
      # Predicting the data with the fitted model
      prediction <- predict(fit, X)

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

#' @export
print.stepwise_selection <- function(object) {
  cat("Stepwise selection object\n")
  cat("-------------------------\n")
  cat(sprintf("Selected predictors: %s\n",
              paste(object$predictors, collapse=", ")))
  cat(sprintf("Error: %f\n", object$error))
  cat(sprintf("Direction: %s", object$direction))
}

#' @export
plot.stepwise_selection <- function(object) {
  plot_data <- data.frame(x = length(object$predictors):object$start_predictors,
                          y = object$errors)

  ggplot(plot_data, aes(x, y)) +
    xlab("Predictor Subset Size") +
    ylab("Error") +
    geom_point(size = 3) +
    geom_line() +
    theme_minimal() +
    ggtitle("Stepwise selection")
}
