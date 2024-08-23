ridge_cv <- function(X, y, m, lambda = NULL, nlambda = 100) {
  # --- Errors and Warnings --- #

  stopifnot("m has to be equal or less the amount of rows of X" = m <= nrow(X))

    #if (nrow(X) %% m != 0) {
  #  warning("nrow(X) is not divisible by m. Will divide X into m nearly equally sized folds.")
  #}

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
  if (is.null(lambda)) {
    ridge_rat_max = 50
    ridge_rat_min = 0.002

    log_min = log(ridge_rat_min)
    log_max = log(ridge_rat_max)

    step = (log_max-log_min)/(nlambda-1)

    log_ridge_rat_vec = seq(log_min,log_max,by = step)
    ridge_rat_vec = exp(log_ridge_rat_vec)

    # scale by sample size n
    lambda = ridge_rat_vec * nlambda
  }

  # Define a progress bar
  pb = txtProgressBar(min = 0, max = m, initial = 0, , style = 3)

  beta <- matrix(nrow = ncol(X), ncol = nlambda)

  # Mean square prediction error of each validation
  mspe_temp <- matrix(NA, m, nlambda)

  # Perform m-fold cross validation
  for(i in 1:m) {
    # Segment data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes, , drop = FALSE]
    trainData <- X[-Indexes, , drop = FALSE]

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

    for (l in 1:nlambda){
      beta[,l] <- solve(Txx + lambda[l] * diagX, Txy, tol = .Machine$double.eps)
    }
    stopifnot("couldn't calculate coefficients. Data is probably poor conditioned"
              = !is.nan(beta))

    # calculate predicted values on original scale
    # Get mean and sd from the training data X
    x_mean <- attr(x_reg, "scaled:center")
    x_sd <- attr(x_reg, "scaled:scale")

    # Get mean of training data y
    y_mean = attr(y_reg, "scaled:center")

    traindata_scaled <- scale(testData, center = x_mean, scale = x_sd) # (testData - x_mean) / x_sd

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

  returnList <- list(nlambda = nlambda,
                     lambda_grid = lambda,
                     min_lambda = min_lambda,
                     m_folds = m,
                     MSPE = MSPE_cv,
                     X = X,
                     y = y,
                     all_coefficients = beta)

  class(returnList) <- c("ridge_cv", "cv")

  return(returnList)
}
