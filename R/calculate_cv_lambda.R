#' Calculates the Lambda with M-fold Cross Validation
#'
#' @param M Split X into M-many equally sized Groups.
#' M = 5 or M = 10 is recommended.
#' @param X Dataset X has the class "tibble".
#' @param Y Dataset Y has the class "tibble".
#' @param lambda Lambda is an atomic vector of double values.
#' All of them are tested to find the optimal lambda with regards to the chosen
#' regmodel
#' @param regmodel Regressionmodel that is used on X and Y. Options are:
#' ridge and lasso
#'
#' @return A double value


## TODO:
##  - implement for actual ridge and lasso models
##  - compatibility with how we formate our input/output
##  - Tests
##  - More input-checking
##  - better over variable names
##  - read in regmodel as expression input thingie

calculate_cv_lambda <- function(M, X, Y, lambda, regmodel = ridge) {

  ## For testing!!! Remove Later!

  ridge <- function(X, Y, lambda) {
    function(values) {
      sum(values)*lambda
    }
  }

  ## -------------------------------

  ## Check input
  M <- as.integer(M)
  stopifnot("M has to be 2 or larger" = M > 1)
  stopifnot("M has to be equal or less the amount of rows of X" = M <= nrow(X))

  stopifnot("lambda must be numeric" = is.numeric(lambda))

  stopifnot("X and Y must have the same amount of rows" = nrow(X) == nrow(Y))

  ## -------------------------------

  n <- nrow(X)
  train_data <- list()

  ## Removing one group at a time
  ##
  ## Example:
  ## x1 y1                                    x1 y1
  ##  1  4                                     1  4
  ##  2  5   -- 3 groups, drop 2nd group -->   3  6
  ##  3  6
  for (i in 1:M) {
    X %>% slice(-(((i-1)*n/M+1):(i*n/M))) -> temp

    train_data[[i]] <- temp
  }

  ## Calculate average loss for each training subset
  ## for the corresponding excluded data group
  cv_res <- tibble(lambda = numeric(), cv = numeric())

  for (l in lambda) {
    temp <- 0
    for (i in seq_along(train_data)) {
      ## Apply model to training subset
      regression_funcs <- ridge(train_data[[i]], Y, lambda = l)

      ## Calculate CV by applying the result of the regression models to th
      ## excluded data and calculating the average squared loss
      X %>%
        slice(((i-1)*n/M+1):(i*n/M)) %>%
        rowwise() %>%
        mutate(loss = (Y[[1]][i]-regression_funcs(c_across()))**2,
               .keep = "none") %>%
        ungroup() %>%
        summarize(sum(loss)) -> tempX


      ## once ridge or lasso is implemented and compatible with predict()
      ##
      ## mutate(loss = list(Y[[1]][i]-predict(regression_funcs(),
      ##                                      newdata = c_across()),
      ##        .keep = "none") ->

      temp <- temp + tempX[[1]]
    }
    temp <- 1/n * temp
    cv_res <- add_row(cv_res, lambda = l, cv = temp)
  }
  print(cv_res)

  return(slice_min(cv_res, order_by = cv))
}
