# --- Test Values and helper functions --- #

n <- 100
p <- 150
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
x1 <- X[ , 1]


y <- rnorm(n)
data <- data.frame(y, X)

log_lambda <- function(nlambda) {
  ridge_rat_max = 50
  ridge_rat_min = 0.002

  log_min = log(ridge_rat_min)
  log_max = log(ridge_rat_max)

  step = (log_max-log_min)/(nlambda-1)

  log_ridge_rat_vec = seq(log_min,log_max,by = step)
  ridge_rat_vec = exp(log_ridge_rat_vec)

  lambda = ridge_rat_vec * nlambda
}


test_that("Lambda input checks work" , {
  formula <- as.formula(y ~ .)

  # --- Errors --- #
  # only one lambda
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = 1 , cv = T))

  # non numeric lambda
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = c(T,F), cv = T))
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = c("blubber", "beast"), cv = T))
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = c(1+2i,3i), cv = T))
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = "plink" , cv = T))
  expect_error(regmodel(formula, data = data, model = "ridge", lambda = c(NaN, NaN), cv = T))

  # --- No Error and correct output --- #

  # custom lambda
  expect_no_error(results <- regmodel(formula, data = data, model = "ridge", lambda = c(1,2,3), cv = T))

  expect_s3_class(results, class = "ridge_cv")
  expect_true(results$min_lambda %in% c(1,2,3))
  expect_equal(results$nlambda, 3)
  expect_equal(results$lambda_grid, c(1,2,3))

  # default lambda
  default_lambda_100 <- log_lambda(100)
  default_lambda_10 <- log_lambda(10)

  expect_no_error(results_100 <- regmodel(formula, data = data, model = "ridge", cv = T))
  expect_equal(default_lambda_100, results_100$lambda)

  expect_no_error(results_10 <- regmodel(formula, data = data, model = "ridge", cv = T, nlambda = 10))
  expect_equal(default_lambda_10, results_10$lambda)


})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
