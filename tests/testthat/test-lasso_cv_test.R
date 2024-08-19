# --- Test Values and helper functions --- #

n <- 100
p <- 150
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
x1 <- X[ , 1]


y <- rnorm(n)
data <- data.frame(y, X)

log_lambda <- function(nlambda) {
  lasso_rat_max = 50
  lasso_rat_min = 0.002

  log_min = log(lasso_rat_min)
  log_max = log(lasso_rat_max)

  step = (log_max-log_min)/(nlambda-1)

  log_lasso_rat_vec = seq(log_min,log_max,by = step)
  lasso_rat_vec = exp(log_lasso_rat_vec)

  lambda = lasso_rat_vec * nlambda
}


test_that("Lambda input checks work" , {
  formula <- as.formula(y ~ .)

  # --- Errors --- #
  # only one lambda
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = 1 , cv = T))

  # non numeric lambda
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = c(T,F), cv = T))
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = c("blubber", "beast"), cv = T))
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = c(1+2i,3i), cv = T))
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = "plink" , cv = T))
  expect_error(regmodel(formula, data = data, model = "lasso", lambda = c(NaN, NaN), cv = T))

  # --- No Error and correct output --- #

  # custom lambda
  expect_no_error(results <- regmodel(formula, data = data, model = "lasso", lambda = c(1,2,3), cv = T))

  expect_s3_class(results, class = "lasso_cv")
  expect_true(results$min_lambda %in% c(1,2,3))
  expect_equal(results$nlambda, 3)
  expect_equal(results$lambda_grid, c(1,2,3))

  # default lambda
  default_lambda_100 <- log_lambda(100)
  default_lambda_10 <- log_lambda(10)

  expect_no_error(results_100 <- regmodel(formula, data = data, model = "lasso", cv = T))
  expect_equal(default_lambda_100, results_100$lambda)

  expect_no_error(results_10 <- regmodel(formula, data = data, model = "lasso", cv = T, nlambda = 10))
  expect_equal(default_lambda_10, results_10$lambda)
})
