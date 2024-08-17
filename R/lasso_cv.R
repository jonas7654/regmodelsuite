#' Calculates the Lambda for LASSO with m-fold Cross Validation
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

      max_abs_beta_diff <- max(abs(beta - beta_old))
      m <- m + 1

    }
    return(as.vector(beta))
  }

  return(lasso_only_lambda)
}



lasso_cv <- function(X, y, m = 10, lambda, iter = 1e-07) {


  # Randomize order of data
  random_order <- sample(1:nrow(X))
  X <- X[random_order,]
  y <- y[random_order]

  # Create m folds
  folds <- cut(seq(1,nrow(X)),breaks=m,labels=FALSE)

  n <- nrow(X)
  p <- ncol(X)

  lambda_min <- rep(0,m)

  if(missing(lambda)) {
    # specify a lambda grid. log transformation to favor smaller values
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

  beta <- matrix(nrow = p, ncol = length(lambda))

  # Mean square prediction error of each validation
  mspe_matrix <- matrix(nrow = m, ncol = length(lambda))

  # Perform m-fold cross validation
  for(i in 1:m){
    # Segment data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes, ]
    trainData <- X[-Indexes, ]

    ytestData <- y[Indexes]
    ytrainData <- y[-Indexes]

    #standardize

    x_reg = scale(trainData) # standardize
    y_reg = scale(ytrainData,scale = FALSE) # demean

    # --- Perform LASSO Regression --- #
    # Funktionsfabrik
    lasso_estimator <- lasso_cv_calculation(x_reg, y_reg)

    # Estimate coefficients for all lambdas

    for (l in 1:length(lambda)) {
      beta[ , l] <- lasso_estimator(lambda[l])
    }

    # calculate predicted values on original scale
    # Get mean and sd from the training data X
    x_mean <- attr(x_reg, "scaled:center")
    x_sd <- attr(x_reg, "scaled:scale")

    # Get mean of training data y
    y_mean = attr(y_reg, "scaled:center")

    # Scale the out of sample X by the mean and sd of the training data X
    test_data_scaled <- (testData - x_mean) / x_sd

    # This is a n x length(lambda) matrix
    out_of_sample_predicted_y <- apply(beta, 2, function(beta) {
      y_mean + test_data_scaled %*% beta
    })

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

  returnList <- list(nridge = length(lambda),
                     lambda_grid = lambda,
                     min_lambda = min_lambda,
                     m_folds = m,
                     MSPE = MSPE_cv)

  class(returnList) <- "lasso_cv"

  return(returnList)
}
