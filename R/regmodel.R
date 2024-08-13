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
# Get varibales from parent environment if data = NULL
# Add option to remove or include intercept ?


regmodel <- function(formula = NULL, data = NULL, model = NULL, lambda = 0,
                     cv = FALSE, ...) {
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
  stopifnot("")
  stopifnot("cv must be a boolean of length one" =
              (is.logical(cv) && length(cv) == 1)
           )

  ########################################################################

  # Extract model matrix and response matrix
  if (!is.null(data)) {
    mf <- model.frame(formula, data = data)
    X <- model.matrix(formula, data = data)
    y <- model.response(mf)
  }

  # Init
  results <- list()
  names <- names(X)




  # Least angle regression call
  if (model == "LAR") {
    results$LAR <- least_angle_regression(X, y)
  }

  # Lasso regression call
  if (model == "lasso") {
    if (cv) {

    }
    else {
      results$lasso <- lasso(X, y, lambda)
    }

  }



  return(results)
}
