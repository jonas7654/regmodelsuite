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
#'  @param ... Additional parameter for the model. Further information see:
#'    \link[regmodelsuite]{lasso}
#'
#' @return Model List
#' @export

# TODO
# Add option to remove or include intercept ?
# Identical return lists for each function
# Add X and y variable to return list in order to call predict efficiently
# Plots
# Better output on return object. Maybe implement summary?
# Compute standardized X and y in regmodel or in estimation functions?
# names(fit$coefficients) <- var_names_x at the end of the function? Requires identical output between functions


regmodel <- function(formula = NULL, data = NULL, model = NULL, lambda = 0,
                     cv = FALSE, intercept = FALSE , ...) {
  # Input checks
  stopifnot("missing formula object" =
              !is.null(formula) || (inherits(formula, "formula")
                                   )
           )

  stopifnot("please specify a model \n
                                                    ridge,
                                                    lasso,
                                                    forward,
                                                    backward,
                                                    LAR            " =
              (is.character(model) && (model %in% c("ridge",
                                                    "lasso",
                                                    "forward",
                                                    "backward",
                                                    "LAR")
                                      )
              )
           )

  stopifnot("lambda must be a positiv number" =
              (is.numeric(lambda) && lambda >= 0)
           )
  stopifnot("cv must be a boolean of length one" =
              (is.logical(cv) && length(cv) == 1)
           )
  stopifnot("data must be NULL or a data frame" =
              is.null(data) || is.data.frame(data)
           )
  stopifnot("Intercept must be TRUE or FALSE" =
              is.logical(intercept)
           )

  ########################################################################


  # Extract data from parent environments up until the globalenv
  if (is.null(data)) {
    var_names <- all.vars(formula)
    data <- sapply(var_names, function(names) {
                                   recursive_data_search(names, parent.frame())
      })

    data <- as.data.frame(data)

    # Check that all variables were collected
    if (ncol(data) != length(var_names)) {
      stop("Couldn't find all variables")
    }

    names(data) <- var_names
  }

  # Intercept handling
  if (!intercept) {
    # remove the intercept (base case)
    t <- terms(formula, data = data)
    formula <- update.formula(formula(t), ~ . + 0)
  }

  # Create model frame
  mf <- model.frame(formula, data = data)
  X <- model.matrix(formula, data = data)
  y <- model.response(mf)



  # Init
  results <- list()

  # extract column names
  var_names_x <- dimnames(X)[[2]]
  names(X) <- var_names_x

  # Ridge call
  if (model == "ridge") {
    results <- ridge(X, y, lambda)
  }

  # Least angle regression call
  if (model == "LAR") {
    fit <- least_angle_regression(X, y)
    names(fit$coefficients) <- var_names_x
    results <- fit
  }

  # Lasso regression call
  if (model == "lasso") {
    if (cv) {
      cv_results <- lasso_cv(X, y, m = 10, nridge = 100)

    }
    else {
      fit <- lasso(X, y, lambda)
      names(fit$coefficients) <- var_names_x
      results <- fit
    }

  }



  return(results)
}
