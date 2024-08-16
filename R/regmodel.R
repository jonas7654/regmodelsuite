#' Wrapper Function for the regmodelsuit package.
#'
#' @param formula A formula object which specifies the model
#' @param data A data frame which contains the corresponding variables
#' @param model Specifies the model to be estimated.
#'   c("ridge","lasso","forward","backward","LAR")
#' @param lambda penalty parameter for ridge and lasso estimation. If cv = TRUE
#'   lambda will be ignored all of them are tested to find the optimal lambda
#'   with regard to the chosen regmodel
#' @param model Logical value which specifies if cross validation for lambda
#'   should be used
#'
#' @return Model List
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
                     cv = FALSE, ...) {

  # Input checks
  stopifnot("please provide a valid formula object" =
              !is.null(formula) && inherits(formula, "formula")
           )
  stopifnot("cv must be length 1 logical value " = (is.logical(cv)) && (length(cv) == 1)
           )

  valid_models <- c("ridge", "lasso", "forward", "backward", "LAR")
  stopifnot("Please select a valid model" =
              is.character(model) && model %in% valid_models)

  if (model %in% c("ridge", "lasso")) {
    if (!cv) {
      if (is.null(lambda)) {
        stop("Please specify a lambda >= 0")
      }
      if (!is.numeric(lambda)  || lambda < 0 || length(lambda) != 1) {
        stop("Lambda must be a positive number")
      }
    }
    # If cv is TRUE
    else {
      if(!is.null(lambda)) {
        stopifnot("lambda values must be positive" = all(lambda > 0))
      }
    }
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
    fit <- ridge(X, y, lambda)
    # Add corresponding variable names to output
    names(fit$coefficients) <- var_names_x

    results <- fit
  }

  # Least angle regression call
  if (model == "LAR") {
    fit <- least_angle_regression(X, y)
    colnames(fit$coefficients) <- var_names_x
    results <- fit
  }

  # Lasso regression call
  if (model == "lasso") {
    if (cv) {
      if (length(lambda) > 1 ) {
        cv_results <- lasso_cv(X, y, m = 10, nridge = length(lambda), lambda = lambda)
        results <- cv_results
      }
      else if (is.null(lambda)) {
        cv_results <- lasso_cv(X, y, m = 10)
        results <- cv_results
      }
      else {
        cv_results <- lasso(X, y, lambda)
        warning("no cross validation was performed")
        results <- cv_results
      }
    }
    else {
      fit <- lasso(X, y, lambda)
      names(fit$coefficients) <- var_names_x
      results <- fit
    }

  }


  if (model %in% c("forward", "backward")) {
    if (model == "forward") {
      # TODO
    }
    else if (model == "backward") {
      # TODO
    }
  }

  # Add function call to results
  results$call <- call
  return(results)
}
