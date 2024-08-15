n <- 100
p <- 150
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
x1 <- X[ , 1]


y <- rnorm(n)
data <- data.frame(y, X)

# Write tests

test_that("lasso works", {
  formula <- as.formula(y ~ .)

  expect_error(regmodel(formula, data = data, model = "lasso", lambda = -1 , cv = F))
  expect_error(regmodel(formula, data = data, model = "lasso", cv = F))


  expect_no_error(regmodel(formula, data = data, model = "lasso", lambda = 0.5,cv = F))
  expect_s3_class(regmodel(formula, data = data, model = "lasso", lambda = 0.5,cv = F), class = "lasso")

})
