#' Wrapper Function for the regmodelsuit package.
#'
#' @param formua A formula object which specifies the model
#' @param data A data frame which contains the corresponding variables
#' @param model Specifies the model to be estimated.
#'   c("ridge","lasso","forward","backward","LAR")
#' @param lambda penalty parameter for ridge and lasso estimation. If cv = TRUE
#'   lambda will be ignored All of them are tested to find the optimal lambda
#'   with regards to the chosen regmodel
#' @param regmodel Logical value which specifies if cross validation for lambda should be used
#'
#' @return Model List

# TODO
# Get varibales from parent environment if data = NULL
# Add option to remove or include intercept ?


regmodel <- function(formula = NULL, data = NULL, model = NULL, lambda = 0,
                     cv = FALSE) {
  # Input checks
  stopifnot("missing formula object" =
              (inherits(formula, "formula")
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

  ########################################################################

  # Init
  results <- list()

  # Extract model matrix and response matrix
  if (!is.null(data)) {
    mf <- model.frame(formula, data = data)
    X <- model.matrix(formula, data = data)
    y <- model.response(mf)
  }

  # Data checks
  stopifnot("X contains invalid entries" = 
              (is.numeric(X) && !any(is.na(X)) && !any(is.nan(X)) && !any(is.infinite(X)))
           )
  stopifnot("y contains invalid entries" = 
              (is.numeric(y) && !any(is.na(y)) && !any(is.nan(y)) && !any(is.infinite(y)))
           )
  stopifnot("Number of rows in X must equal length of y" = 
              (nrow(X) == length(y))
           )




  if (model == "LAR") {
    results$LAR <- least_angle_regression(X, y)
  }

  if (model == "ridge") {
    if(length(lambda)>1){
      results$ridge <- lapply(lambda, function(lambda){
        return(ridge(X, y, lambda))
      })
    }else{
      results$ridge <- ridge(X, y, lambda)
    }
  }



  return(results)
}
