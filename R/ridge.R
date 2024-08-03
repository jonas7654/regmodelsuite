#' Calculates the ridge regression
#'
#' This function calculates the coefficients of a ridge regression model
#' for a given predictor matrix `X` and response vector `Y`.
#'
#' @param X A matrix of predictor variables.
#' @param Y A vector of response values.
#' @param lambda A numeric value for the regularization parameter.
#'
#' @return A vector of ridge regression coefficients.
#' 
#' @example 
#' set.seed(21)
#' X <- matrix(runif(100 * 10), 100, 10)
#' Y <- X %*% runif(10, min = -1, max = 1) + rnorm(100, mean = 0, sd = 0.3)
#' ridge(X,Y,0.1)


ridge <- function(X,Y, lambda = 0){
  ridge <- .Internal(La_solve(t(X) %*% X + lambda * diag(ncol(X)), t(X) %*% Y, tol = .Machine$double.eps))
  return(ridge)
}





#test wie in Richter (am ende entfernen, beispiel von @example):
set.seed(21) 
n <- 100
d <- 10
o <- 0.3
X <- matrix(runif(n * d), n, d) #unabhängig+gleichverteilt auf [0,1]
real_ridge <- runif(d, min = -1, max = 1) #die echten Werte für Beta
Y <- X %*% real_ridge + rnorm(n, mean = 0, sd = o) #Y mit normalverteiltem Messfehler N(0,o^2)
test_ridge <- ridge(X,Y,0)
as.vector(test_ridge)
real_ridge
