#' Calculates the Lambda for Ridge with m-fold Cross Validation
#'
#' @param X Dataset X has the class "matrix".
#' @param y Target variable y has the class "matrix".
#' @param M Split X into M-many equally sized Groups.
#' m = 5 or m = 10 is recommended. The default is m = 10.
#' @param lambda Lambda is an atomic vector of double values.
#' cross_validation will find the optimal lambda among those. If no lambda is chosen,
#' then a default range of lambdas will be tested.
#'
#' @return A list with the mean squared prediction error, the optimal lambda and
#' the coefficients of the chosen regression model with the optimal lambda.


################################################################################
# Version 3


cross_validation <- function(X, y, m = 10, lambda) {
  # --- Errors and Warnings --- #

  m <- as.integer(m)
  stopifnot("m has to be 2 or larger" = m > 1)
  stopifnot("m has to be equal or less the amount of rows of X" = m <= nrow(X))

  if (nrow(X) %% m != 0) {
    warning("nrow(X) is not divisible by m. Will divide X into m nearly equally sized folds.")
  }

  if (!missing(lambda)) {
    stopifnot("lambda must be numeric" = is.numeric(lambda))
    stopifnot("lambda must be a vector" = is.vector(lambda))
    stopifnot("lambda must not contain negative values" = all(lambda >= 0))

    if (any(lambda == 0)) {
      warning("lambda contains 0. Ridge regression with lambda = 0 is equivalent to Least Squares Regression.")
      OLS <- lm.fit(X, y)$coefficients
      if (ncol(X) >= nrow(X)) {
        warning("The matrix X suffers from multicollinearity")
      }
      return(as.vector(OLS))
    }
  }

  stopifnot("X and y must have the same amount of rows" = nrow(X) == nrow(y))

  # Randomize order of data
  random_order <- sample(1:nrow(X))
  X <- X[random_order,]
  y <- y[random_order]

  # --- Define Folds --- #
  # Creates m (nearly) equally sized folds.
  # Fold lengths differ by at most 1.
  folds <- cut(seq(1,nrow(X)),breaks = m,labels=FALSE)

  # --- Default Lambda --- #
  # If no lambda is provided, specify a lambda grid.
  # log transformation to favor smaller values
  if (missing(lambda)) {
    ridge_rat_max = 50
    ridge_rat_min = 0.002

    log_min = log(ridge_rat_min)
    log_max = log(ridge_rat_max)

    step = (log_max-log_min)/(100-1)

    log_ridge_rat_vec = seq(log_min,log_max,by = step)
    ridge_rat_vec = exp(log_ridge_rat_vec)

    # scale by sample size n
    lambda = ridge_rat_vec * 100
  }

  # Define a progress bar
  pb = txtProgressBar(min = 0, max = m, initial = 0, , style = 3)

  beta <- matrix(nrow = ncol(X), ncol = length(lambda))

  # Mean square prediction error of each validation
  mspe_temp <- matrix(NA, m, length(lambda))

  # Perform m-fold cross validation
  for(i in 1:m) {
    # Segment data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes,]
    trainData <- X[-Indexes,]

    ytestData <- y[Indexes]
    ytrainData <- y[-Indexes]

    # standardize
    x_reg <- scale(trainData) # Intercept should be removed beforehand!
    y_reg <- scale(ytrainData,scale = FALSE)

    # --- Perform ridge regression --- #

    # precompute invariants
    Txx <- t(x_reg) %*% x_reg
    diagX <- diag(ncol(x_reg))
    Txy <- t(x_reg) %*% y_reg

    for (l in 1:length(lambda)){
      beta[,l] <- solve(Txx + lambda[l] * diagX, Txy, tol = .Machine$double.eps)
    }

    # calculate predicted values on original scale
    # Get mean and sd from the training data X
    x_mean <- attr(x_reg, "scaled:center")
    x_sd <- attr(x_reg, "scaled:scale")

    # Get mean of training data y
    y_mean = attr(y_reg, "scaled:center")

    traindata_scaled <- (testData - x_mean) / x_sd

    y_predicted <- as.matrix(apply(beta,
                                   2 ,
                                   function(x) {traindata_scaled %*% x},
                                   simplify = TRUE))

    # Rescale predicated values
    # This is a n x length(lambda) matrix
    out_of_sample_predicted_y <- as.matrix(apply(beta, 2, function(beta) {
      y_mean + traindata_scaled %*% beta
    }))

    # Calculate prediction error
    mspe_temp[i , ] <- apply(out_of_sample_predicted_y, 2, function(pred) {
      mean((pred - ytestData)^2)},
      simplify = TRUE)

    # Update progress bar
    setTxtProgressBar(pb, i)

  }
  # Close progress bar
  close(pb)

  MSPE_cv = colMeans(mspe_temp)
  min_lambda_index <- which.min(MSPE_cv)

  min_lambda = lambda[min_lambda_index]

  returnList <- list(nridge = length(lambda),
                     lambda_grid = lambda,
                     min_lambda = min_lambda,
                     m_folds = m,
                     MSPE = MSPE_cv)

  class(returnList) <- "ridge_cv"

  return(returnList)
}
