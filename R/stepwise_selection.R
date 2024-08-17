#' Finds a subset of predictors using forward selection
#'
#' @param X Dataset X
#' @param y Dataset y
#' @param n_predicors Amount of predictors to select
#' @param model Modeling function
#' @param verbose Whether to print information of the selection process
#'
#' @return A stepwise_selection object
forward_selection <- function(X, y, n_predictors, model = lm,
                               verbose = TRUE) {

  stopifnot("n_predictors has to be bigger than 0." = n_predictors > 0)

  unused_predictors <- colnames(X)
  used_predictors <- c()
  iterations <- min(n_predictors, ncol(X))

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y))

  formula_start <- paste(colnames(X)[ncol(X)], "~")

  for(i in 1:iterations) {

    if(verbose)
      cat(sprintf("%dth iteration:\n", i))

    best_loss <- NULL

    for(p in seq_along(unused_predictors)) {

      # Creating new combination of predictors to test
      cur_predictors <- c(used_predictors, unused_predictors[p])

      # Creating formula object with the new combination
      formula <- as.formula(paste(formula_start,
                                  paste(cur_predictors, collapse="+")))

      fit <- model(formula, X)

      # Predicting the data with the fitted model
      prediction <- predict(fit, X)

      # Calculating squared loss
      loss <- sum((y - prediction) ^ 2) / length(prediction)

      if(verbose) {
        cat(sprintf("  + %s: %f\n", unused_predictors[p], loss))
      }

      # Checking if the loss is currently the best
      if(loss < best_loss || is.null(best_loss)) {
        best_loss <- loss
        best_predictor <- p
        best_fit <- fit
      }
    }

    if(verbose) {
      cat(sprintf("Added predicor '%s', Loss: %f\n\n",
                    unused_predictors[best_predictor], best_loss))
    }

    # Removing and adding the selected predictor from the lists
    used_predictors <- c(used_predictors, unused_predictors[best_predictor])
    unused_predictors <- unused_predictors[-best_predictor]
  }

  results <- list(model = best_fit,
                  predictors = used_predictors,
                  loss = best_loss,
                  direction = "forward")

  class(results) <- "stepwise_selection"

  results
}

#' Finds a subset of predictors using backward selection
#'
#' @param X Dataset X
#' @param y Dataset y
#' @param n_predicors Amount of predictors to select
#' @param model Modeling function
#' @param verbose Whether to print information of the selection process
#'
#' @return A stepwise_selection object
backward_selection <- function(X, y, n_predictors, model = lm, verbose = TRUE) {

  stopifnot("n_predictors has to be bigger than 0." = n_predictors > 0)

  stopifnot("More data points than predictors required."
            = nrow(X) > ncol(X))

  used_predictors <- colnames(X)
  iterations <- length(used_predictors) - n_predictors + 1

  # Combining X and y and converting to data frame
  X <- data.frame(cbind(X, y))

  formula_start <- paste(colnames(X)[ncol(X)], "~")

  for(i in 1:iterations) {

    best_loss <- NULL

    # Creating formula object with the new combination
    formula <- as.formula(paste(formula_start,
                                  paste(used_predictors, collapse="+")))

    fit <- model(formula, X)

    # Predicting the data with the fitted model
    prediction <- predict(fit, X)

    # Calculating squared loss
    loss <- sum((y - prediction) ^ 2) / length(prediction)

    if(verbose)
      cat(sprintf("%f\n\n", loss))

    # The last iteration is just for fitting the model and calculating the loss
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
      cat(sprintf("Removed predictor '%s' with Z-score: %f. Loss: ",
                  lowest_name, lowest_score))

    # Removing the predictor with the lowest Z-score from the used predictors
    used_predictors <- used_predictors[-lowest_which]
  }

  results <- list(model = fit,
                  predictors = used_predictors,
                  loss = loss,
                  direction = "backward")

  class(results) <- "stepwise_selection"

  results
}
