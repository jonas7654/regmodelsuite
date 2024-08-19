#' Wrapper function for the regmodelsuit package
#'
#' @param formula A formula object which specifies the model
#' @param data A data frame which contains the corresponding variables. If no
#'   data frame is provided then the function will recursively search for
#'   variables specified by the formula. The variables have to be defined in one
#'   of the functions parent environments up to the global environment.
#' @param model A string with the model to be estimated. Following models are
#'   supported
#' \itemize{
#'    \item "ridge"
#'    \item "lasso"
#'    \item "forward"
#'    \item "backward"
#'    \item "LAR"
#' }
#' @param lambda A numeric value which defines the penalty parameter for ridge
#'   and lasso estimation. \cr If using cross validation, it is a numeric vector
#'   with at least length 2. If no lambda is given, cross validation will
#'   generate default values. The generated lambda grid is scaled by the total
#'   number of lambdas (`nlambda`) to account for sample size.
#' @param cv A logical value which specifies if cross validation should be used.
#' @param m An integer for the amount of folds when using cross validation.
#' @param nlambda An integer that defines the amount of values that are
#'   generated as default lambdas in cross validation.
#'
#' @details If `lambda` is `NULL`, a grid of lambda values is automatically
#'   generated.
#'
#'   The grid creation process is as follows:
#'
#' \itemize{
#' \item A range of ratios is specified, from 0.002 to 50.
#' \item The logarithms of these minimum and maximum values are computed to
#' emphasize smaller lambda values.
#' \item A sequence of evenly spaced values is generated on the logarithmic
#' scale.
#' \item The sequence is exponentiated to produce the actual lambda values.
#' \item Finally, these lambda values are scaled by `nlambda`, the number of
#' lambda values generated, to finalize the grid.
#' }
#'
#'   For more details regarding cross validation and the variables see the cv
#'   vignette: \cr \code{vignette("cv", package = "regmodelsuite")}
#'
#' @return S3 object of the chosen regression model class, containing different
#'   informations.
#'
#' @importFrom stats as.formula model.frame model.matrix.lm model.response
#'   complete.cases
#'
#' @export

# TODO
# Identical return lists for each function
# Add X and y variable to return list in order to call predict efficiently
# Plots
# Better output on return object. Maybe implement summary?
# names(fit$coefficients) <- var_names_x at the end of the function? Requires identical output between functions
# include r-squared into output via calculate_R2...
# explain lambda grid

regmodel <- function(formula = NULL, data = NULL, model = NULL, lambda = NULL,
                     cv = FALSE, m = 10, nlambda = 100, n_predictors, ...) {

  # Input checks
  stopifnot("Please provide a valid formula object" =
              !is.null(formula) && inherits(formula, "formula")
           )
  stopifnot("cv must be either TRUE or FALSE" = is.logical(cv))

  stopifnot("cv must be a single logical value" = length(cv) == 1)

  ## better ?
  valid_models <- c("ridge", "lasso", "forward", "backward", "LAR")
  stopifnot("Please select a valid model" =
              is.character(model) && model %in% valid_models)

  if (model %in% c("ridge", "lasso")) {
    if (!cv) {
      stopifnot("Please specify a lambda" = !is.null(lambda))

      stopifnot("lambda must be numeric" = is.numeric(lambda))
      stopifnot("lambda must be a single number" = length(lambda) == 1)
      stopifnot("lambda may not be negative" = lambda >= 0)
    }
    # otherwise, cv is TRUE
    # so, we can immediately check for lambda
    else if(!is.null(lambda)) {
        stopifnot("lambda must be numeric" = is.numeric(lambda))
        stopifnot("lambda values must be a vector" = is.vector(lambda))
        stopifnot("lambda values may not be negative" = all(lambda >= 0))
        stopifnot("Provide either a vector with length >= 2 or no lambda for cross validation.
    See ?ridge_cv or ?lasso_cv for details" =
                    length(lambda) > 1)

        stopifnot("m has to be numeric" = is.numeric(m))
        m <- as.integer(m)
        stopifnot("m has to be 2 or larger" = m > 1)
    }
    else {
      # if user chooses cv but default lambdas
      stopifnot("nlambda must be numeric" = is.numeric(nlambda))
      stopifnot("nlambda must be positive" = nlambda > 1)
      stopifnot("nlambda must be a single number" = length(nlambda) == 1)
      nlambda <- as.integer(nlambda)
    }
  }
  else if (cv) {
    stop("Please choose ridge or lasso if choosing cv")
  }
  else if (model %in% c("forward", "backward")) {
    stopifnot("n_predictors has to be bigger than 0." = n_predictors > 0)
    stopifnot("n_predictors must be numeric" = is.numeric(n_predictors))
    stopifnot("n_predictors must be a single number" = length(n_predictors) == 1)
  }

  if (!is.null(data)) {
    stopifnot("data must be of type data.frame" = is.data.frame(data))
  }


  ########################################################################


  # Extract data from parent environments up until the globalenv
  var_names <- all.vars(formula)
  if (is.null(data)) {
    data <- sapply(var_names, function(names) {
                                   recursive_data_search(names, parent.frame())
      })

    data <- as.data.frame(as.matrix(data))

    # Check that all variables were collected
    if (ncol(data) != length(var_names)) {
      stop("Couldn't find all variables")
    }

    names(data) <- var_names
  }



  # Create model frame
  formula_without_intercept <- as.formula(paste(deparse(formula) , "+ 0"))

  # keep NA's in order to generate a warning before removing them
  mf <- model.frame(formula_without_intercept, data = data, na.action = "na.pass")
  X <- model.matrix.lm(formula_without_intercept,
                       data = data, na.action = "na.pass") # removed intercept
  y <- model.response(mf)


  ### Handle missing values ###

  # Find complete rows and drop NA rows
  complete_rows <- complete.cases(X)
  if (sum(complete_rows) == 0) {
    stop("The dataframe has only missing values")
  }
  if (sum(!complete_rows) > 0) {
    warning(paste(sum(!complete_rows), "rows with missing values"))
  }
  X <- X[complete_rows , , drop = FALSE]
  y <- y[complete_rows]



  # This is probably not needed anymore since X[, , drop = FALSE] is set
  # Edge case if X contains only one variable
  if (is.null(dim(X))) {
    X <- as.matrix(X)
    dim(X) <- c(length(X), 1)
  }

  # Init
  results <- list()
  call <- match.call()

  # extract column names
  var_names_x <- dimnames(X)[[2]]
  colnames(X) <- var_names_x

  # Ridge call
  if (model == "ridge") {
    if (cv) {
      if (!is.null(lambda)) {
        cv_results <- ridge_cv(X, y, m = 10, nlambda = length(lambda), lambda = lambda)
        results <- cv_results
      }
      else {
        cv_results <- ridge_cv(X, y, m = 10, nlambda = nlambda)
        results <- cv_results
      }
    }
    else {
      fit <- ridge(X, y, lambda)
      # Add corresponding variable names to output
      names(fit$coefficients) <- var_names_x

      results <- fit
    }
  }
  # Least angle regression call
  else if (model == "LAR") {
    fit <- least_angle_regression(X, y)
    colnames(fit$coefficients) <- var_names_x
    results <- fit
  }
  # Lasso regression call
  else if (model == "lasso") {
    if (cv) {
      if (!is.null(lambda)) {
        cv_results <- lasso_cv(X, y, m = 10, nlambda = length(lambda), lambda = lambda)
        results <- cv_results
      }
      else {
        cv_results <- lasso_cv(X, y, m = 10, nlambda = nlambda)
        results <- cv_results
      }
    }
    else {
      fit <- lasso(X, y, lambda)
      names(fit$coefficients) <- var_names_x
      fit$active_variables <- names(fit$coefficients[fit$coefficients != 0])
      fit$inactive_variables <- names(fit$coefficients[fit$coefficients == 0])
      results <- fit
    }
  }
  else if(model == "forward") {
    results <- forward_selection(X, y, n_predictors = n_predictors, ...)
  }
  else if(model == "backward") {
    results <- backward_selection(X, y, n_predictors = n_predictors, ...)
  }

  # Add function call to results
  results$call <- call
  return(results)
}
