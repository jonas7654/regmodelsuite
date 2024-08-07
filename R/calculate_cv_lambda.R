#' Calculates the Lambda with M-fold Cross Validation
#'
#' @param X Dataset X has the class "matrix".
#' @param Y Target variable Y has the class "matrix".
#' @param M Split X into M-many equally sized Groups.
#' M = 5 or M = 10 is recommended.
#' @param lambda Lambda is an atomic vector of double values.
#' cross_validation will find the optimal lambda with regards to the chosen
#' regression model.
#' @param regmodel Regression model that is used on the data during Cross Validation.
#' Options are:
#' - ridge
#' - lasso
#'
#' @return A list with the mean squared prediction error, the optimal lambda and
#' the coefficients of the chosen regression model with the optimal lambda.


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


################################################################################
# Jonas Version #


cv.ridge <- function(X,y, m) {

  #browser()
  # Create Folds
  folds <- cut(seq(1,nrow(X)),breaks = m,labels=FALSE)

  n <- nrow(X)
  nridge <- 100
  lambda_min <- rep(0,m)

  # lambda # :TODO : Welches Grid ist hier sinnvoll?
  ridge_rat_max <- 50
  ridge_rat_min <- 0.002
  # Logarithm to favour smaller values more than larger values since greater Lambdas shrink the estimator
  log_min <- log(ridge_rat_min)
  log_max <- log(ridge_rat_max)

  step <- (log_max-log_min)/(nridge-1)

  log_ridge_rat_vec <- seq(log_min,log_max,by = step)
  ridge_rat_vec <- exp(log_ridge_rat_vec)
  # lambda <- ridge_rat_vec*n
  lambda <- 10^seq(-4, 4, length.out = 100)

  # result vectors
  mspe_temp <- matrix(NA, m, nridge)

  #Perform cross validation
  for(i in 1:m){
    #Segment data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes,]
    trainData <- X[-Indexes,]

    ytestData <- y[Indexes]
    ytrainData <- y[-Indexes]

    #standardize
    x_reg <- scale(trainData) # Intercept should be removed beforehand!
    y_reg <- scale(ytrainData,scale = FALSE)

    #ridge regression (Hier könnte man dann auch einfach ridge(x,y,lambda) callen)
    beta <- matrix(nrow = ncol(trainData), ncol = length(lambda))
    for (l in 1:nridge){
      beta[,l] <- solve(t(x_reg) %*% x_reg + lambda[l]*diag(ncol(x_reg)), t(x_reg) %*% y_reg)
    }

    # calculate the fitted values on the standardized scale using the testData
    # Standardize the testData first
    sd_trainData <- apply(trainData,
                          2,
                          function(x) {sd(x)},
                          simplify = TRUE)

    traindata_scaled <- (testData - colMeans(trainData)) / sd_trainData

    y_predicted <- apply(beta,
                         2 ,
                         function(beta) {traindata_scaled %*% beta},
                         simplify = TRUE)


    # rescale predicted values
    y_predicted_original_scale <- y_predicted + mean(ytrainData)

    # Calculate mean squared prediction error for each run (i = 1 : m)
    mspe_temp[i, ] <- apply(y_predicted_original_scale,
                            2,
                            function(x) {mean((x - ytestData)^2)},
                            simplify = TRUE
    )
  }

  # Compute results
  MSPE_final <- colMeans(mspe_temp)
  mspe_min_index <- which.min(MSPE_final)


  # Estimate the model once again with the optimal lambda
  beta_final <- solve(t(X) %*% X + lambda[mspe_min_index] * diag(ncol(X)), t(X) %*% y)

  # Save results
  result_list <- list(
    cv.MSPE = MSPE_final[mspe_min_index],
    cv.lambda = lambda[mspe_min_index],
    final_model_coef = beta_final
  )

  return(result_list)
}

################################################################################
# Version 3
#
# TODO:
# - Should the default be 100 lambdas?
# - Implement with actual Ridge and LASSO functions
# - compatibility with how we format our input/output ?
#   -> and tests to ensure correct format of X and Y
# - (Optimize ?)

cross_validation <- function(X, Y, M, lambda, regmodel = ridge) {
  # --- Errors and Warnings --- #

  M <- as.integer(M)
  stopifnot("M has to be 2 or larger" = M > 1)
  stopifnot("M has to be equal or less the amount of rows of X" = M <= nrow(X))

  if (nrow(X) %% M != 0) {
    warning("nrow(X) is not divisible by M. Will divide X into M nearly equally sized folds.")
  }

  if (!missing(lambda)) {
    stopifnot("lambda must be numeric" = is.numeric(lambda))
    stopifnot("lambda must be a vector" = is.vector(lambda))
    stopifnot("lambda must not contain negative values" = all(lambda >= 0))

    if (any(lambda == 0) && rlang::enexpr(regmodel) == "ridge") {
      warning("lambda contains 0. Ridge regression with lambda = 0 is equivalent to Least Squares Regression.")
    }
  }

  stopifnot("X and Y must have the same amount of rows" = nrow(X) == nrow(Y))

  stopifnot("regmodel must be either ridge or lasso" =
              rlang::enexpr(regmodel) == "ridge"|
              rlang::enexpr(regmodel) == "lasso")


  # --- Define Folds --- #
  # Creates m (nearly) equally sized folds.
  # Folds differ by at most 1.
  folds <- cut(seq(1,nrow(X)),breaks = M,labels=FALSE)

  # --- Default Lambda --- #
  # If no lambda is provided, use an arbitrary range of lambdas.
  if (missing(lambda)) {
    # Options 1:
    lambda <- 10^seq(-4, 4, length.out = 100)
    # Option 2:
    # lambda <- seq(10^-4, 10^4, length.out = 100)
    # Option 3: ???
  }

  # Mean square prediction error of each validation
  mspe_temp <- matrix(NA, M, length(lambda))

  # Perform m-fold cross validation
  for(i in 1:M) {
    # Segment data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes,]
    trainData <- X[-Indexes,]

    ytestData <- Y[Indexes]
    ytrainData <- Y[-Indexes]

    # --- Perform chosen regression --- #
    if (rlang::enexpr(regmodel) == "ridge") {
      #standardize
      x_reg <- scale(trainData) # Intercept should be removed beforehand!
      y_reg <- scale(ytrainData,scale = FALSE)

      #ridge regression (Hier könnte man dann auch einfach ridge(x,y,lambda) callen)
      beta <- matrix(nrow = ncol(trainData), ncol = length(lambda))
      for (l in 1:length(lambda)){
        beta[,l] <- solve(t(x_reg) %*% x_reg + lambda[l]*diag(ncol(x_reg)), t(x_reg) %*% y_reg)
      }

      # calculate the fitted values on the standardized scale using the testData
      # Standardize the testData first
      sd_trainData <- apply(trainData,
                            2,
                            function(x) {sd(x)},
                            simplify = TRUE)

      traindata_scaled <- (testData - colMeans(trainData)) / sd_trainData

      y_predicted <- apply(beta,
                           2 ,
                           function(beta) {traindata_scaled %*% beta},
                           simplify = TRUE)


      # rescale predicted values
      y_predicted_original_scale <- y_predicted + mean(ytrainData)
    }
    else if (rlang::enexpr(regmodel == "lasso")) {

    }


    # Calculate mean squared prediction error for each run (i = 1 : m)
    mspe_temp[i, ] <- apply(y_predicted_original_scale,
                            2,
                            function(x) {mean((x - ytestData)^2)},
                            simplify = TRUE
    )
  }

  # Compute results
  MSPE_final <- colMeans(mspe_temp)
  mspe_min_index <- which.min(MSPE_final)


  # Estimate the model once again with the optimal lambda
  beta_final <- solve(t(X) %*% X + lambda[mspe_min_index] * diag(ncol(X)), t(X) %*% Y)

  # Save results
  result_list <- list(
    cv.MSPE = MSPE_final[mspe_min_index],
    cv.lambda = lambda[mspe_min_index],
    final_model_coef = beta_final
  )

  return(result_list)
}
