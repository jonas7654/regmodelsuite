n <- 25
p <- 50

true_beta <- rnorm(p, mean = 10, sd = 2)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
x1 <- X[ , 1]


y <- X %*% true_beta + rnorm(n)
data <- data.frame(y, X)
formula <- as.formula(y ~ .)

# Write tests

test_that("lasso works", {

  # Test with p > n
  lasso_fit <- regmodel(formula, data, model = "lasso", lambda = 0.02, cv = F)

  expect_s3_class(lasso_fit, "lasso")
  expect_in(names())


})
