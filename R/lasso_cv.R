


lasso_cv_calculation <- function(X, y, lambda, n, p) {

  # Init
  X_scaled <- scale(X, scale = F)
  beta <- double(p)
  max_abs_beta_diff <- Inf
  mean_X_scaled_squared <- mean(X_scaled^2)
  m <- 0L


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

  return(beta)
}



lasso_cv <- function(X, y, m = 10, iter = 1e-07, nridge = 100) {

  # Create m folds
  folds <- cut(seq(1,nrow(X)),breaks=m,labels=FALSE)

  n_insample = nrow(X)

  lambda_min = rep(0,m)

  MSPE_RIDGE = matrix(nrow = m, ncol = nridge)

  mspe_rslt = matrix(nrow = m,ncol = nridge)

  # specify a lambda grid.
  ridge_rat_max = 50
  ridge_rat_min = 0.002

  log_min = log(ridge_rat_min)
  log_max = log(ridge_rat_max)

  step = (log_max-log_min)/(nridge-1)

  log_ridge_rat_vec = seq(log_min,log_max,by = step)
  ridge_rat_vec = exp(log_ridge_rat_vec)
  lambda = ridge_rat_vec*nobs_insample


  #Perform cross validation
  for(i in 1:m){
    #Segement data
    Indexes <- which(folds==i,arr.ind=TRUE)

    testData <- X[Indexes, ]
    trainData <- X[-Indexes, ]

    ytestData <- y[Indexes]
    ytrainData <- y[-Indexes]

    #standardize

    x_reg = scale(trainData, scale = F)
    y_reg = scale(ytrainData,scale = FALSE)


    # Estimate coefficients for all lambdas

    beta <- apply(lambda,
                  2 ,
                  lasso_cv_calculation(x_reg,
                                       y_reg,
                                       lambda = lambda,
                                       n = length(x_reg),
                                       p = ncol(x_reg))
                 )

    for (l in 1:nridge){
      beta[,l] = solve(t(x_reg) %*% x_reg + lambda[l]*diag(ncol(x_reg)), t(x_reg) %*% y_) #without INTERCEPT
    }

    # calculate formula 14.5 (predicted values on raw scale)
    s = matrix(nrow = nrow(testData), ncol = ncol(testData))
    y_mean = mean(ytrainData)

    for (j in 1:nridge){
      for (k in 1:ncol(testData)){
        s[,k] = beta [k,j] * ((testData[,k] - mean(trainData[,k])) / sd(trainData[,k]))
      }

      s_sum = rowSums(s,na.rm = TRUE)

      y_pred = y_mean + s_sum

      mspe = mean((ytestData - y_pred)^2)


      MSPE_RIDGE[i,j] = mspe
    }
  }

  MSPE_cv_ridge = colMeans(MSPE_RIDGE)
  lambda_min_ridge = lambda[which.min(MSPE_cv_ridge)]
  MSPE_cv_ridge_min = min(MSPE_cv_ridge)
  RMSPE_cv_ridge = sqrt(MSPE_cv_ridge)
}
