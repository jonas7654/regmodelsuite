ridge <- function(X,Y, lambda = 0){
  #inputcheck
  stopifnot("X and Y must have the same amount of rows" = nrow(X) == nrow(Y))
  stopifnot("lambda must be numeric" = is.numeric(lambda))
  stopifnot("lambda has to be greater than zero" = lambda >= 0)
  
  
  E <- diag(ncol(X))
  E[1,1]<-0
  ridge <- .Internal(La_solve(t(X) %*% X + lambda * nrow(X) * E, t(X) %*% Y, tol = .Machine$double.eps))
  return(ridge)
}
#n im Buch = Anzahl der Testdaten, Matrix E muss 0 in erster Zeile haben

#test wie im Buch:
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
