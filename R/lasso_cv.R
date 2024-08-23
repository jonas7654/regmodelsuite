#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom stats lm.fit

# This is just a helper function for a more efficient calculation
lasso_cv_calculation <- function(X, y, tol = 1e-07) {

  # Init
  n <- nrow(X)
  p <- ncol(X)

  X_scaled <- X # X is already scaled
  max_abs_beta_diff <- Inf
  mean_X_scaled_squared <- mean(X_scaled^2)
  m <- 0L

  lasso_only_lambda <- function(lambda) {

    if (lambda == 0) {
      OLS <- lm.fit(X, y)$coefficients
      if (ncol(X) >= nrow(X)) {
        warning("The matrix X suffers from multicollinearity")
      }
      return(as.vector(OLS))
    }

    beta <- double(p)

    while(max_abs_beta_diff > tol) {

      beta_old <- beta
      for (j in 1:p) {
        # 1
        r <- y - X_scaled[ , -j] %*% beta[-j]

        # 2
        beta_j_tilde <- mean(X_scaled[ , j] * r)

        # 3
        beta_j_next <- ifelse(j == 1, beta_j_tilde,
                              (1 / (mean(X_scaled[ , j]^2))) *
                                sign(beta_j_tilde) *
                                max(0, (abs(beta_j_tilde) - (lambda/2)))
        )
        beta[j] <- beta_j_next

      }

      max_abs_beta_diff <- max(abs(beta_old - beta_old))
      m <- m + 1

    }
    return(as.vector(beta))
  }

  return(lasso_only_lambda)
}



lasso_cv <- function(X, y, m, lambda = NULL, nlambda = 100, iter = 1e-06) {
  # --- Errors and Warnings --- #
  stopifnot("m has to be equal or less the amount of rows of X" = m <= nrow(X))

  if (nrow(X) %% m != 0) {
    warning("nrow(X) is not divisible by m. Will divide X into m nearly equally sized folds.")
  }

  stopifnot("X and y must have the same amount of rows" = nrow(X) == nrow(y))

  # Randomize order of data
  random_order <- sample(1:nrow(X))
  X <- X[random_order,]
  y <- y[random_order]

  # Create m folds
  folds <- cut(seq(1,nrow(X)),breaks=m,labels=FALSE)

  n <- nrow(X)
  p <- ncol(X)

  lambda_min <- rep(0,m)

  if(is.null(lambda)) {
    # specify a lambda grid. log transformation to favor smaller values
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

  beta <- matrix(nrow = p, ncol = nlambda)

  # Mean square prediction error of each validation
  mspe_matrix <- matrix(nrow = m, ncol = nlambda)

  # Perform m-fold cross validation
  for(i in 1:m){


    # Segment data
    Indexes <- which(folds==i, arr.ind=TRUE)

    testData <- X[Indexes, , drop = FALSE]
    trainData <- X[-Indexes, , drop = FALSE]

    ytestData <- y[Indexes]
    ytrainData <- y[-Indexes]

    #standardize

    x_reg <- scale(trainData) # standardize
    y_reg <- scale(ytrainData,scale = FALSE) # demean

    # --- Perform LASSO Regression --- #

    # Funktionsfabrik
    #lasso_estimator <- lasso_cv_calculation(x_reg, y_reg)

    # Estimate coefficients for all lambdas

    for (l in 1:nlambda) {
      beta[ , l] <- lasso(x_reg, y_reg, lambda[l])$coefficients   #lasso_estimator(lambda[l])
    }

    stopifnot("couldn't calculate coefficients. Data is probably poor conditioned"
              = !is.nan(beta))

    # calculate predicted values on original scale
    # Get mean and sd from the training data X
    x_mean <- attr(x_reg, "scaled:center")
    x_sd <- attr(x_reg, "scaled:scale")

    # Get mean of training data y
    y_mean = attr(y_reg, "scaled:center")

    # Scale the out of sample X by the mean and sd of the training data X
    testdata_scaled <- scale(testData, center = x_mean, scale = x_sd) # (testData - x_mean) / x_sd

    # This is a n x length(lambda) matrix
    out_of_sample_predicted_y <- as.matrix(apply(beta, 2, function(beta) {
      y_mean + testdata_scaled %*% beta
    }))

    # Calculate prediction error
    mspe_matrix[i , ] <- apply(out_of_sample_predicted_y, 2, function(pred) {
      mean((pred - ytestData)^2)
    })

    # Update progress bar
    setTxtProgressBar(pb, i)

  }
  # Close progress bar
  close(pb)

  MSPE_cv = colMeans(mspe_matrix)
  min_lambda_index <- which.min(MSPE_cv)

  min_lambda = lambda[min_lambda_index]

  returnList <- list(nlambda = nlambda,
                     lambda_grid = lambda,
                     min_lambda = min_lambda,
                     m_folds = m,
                     MSPE = MSPE_cv,
                     X = X,
                     y = y)

  class(returnList) <- c("lasso_cv", "cv")

  return(returnList)
}
