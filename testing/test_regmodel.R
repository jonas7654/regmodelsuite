library(testthat)

# Define a simple dataset for testing
set.seed(123)
n <- 100
X <- matrix(rnorm(n * 3), ncol = 3);colnames(X) <- paste0("x", 1:3)
y <- rnorm(n)

# Testing lasso_cv function
test_that("lasso_cv function returns correct structure and values", {

  # Test case: Cross-validation with a specified lambda grid
  lambda_values <- c(0.1, 1, 10)
  cv_results <- regmodel(y ~ x1 + x2 + x3, data = as.data.frame(cbind(y,X)), model = "lasso", lambda = lambda_values, cv = T)

  expect_true(is.list(cv_results))
  expect_true(all(c("nridge", "lambda_grid", "min_lambda", "m_folds", "MSPE") %in% names(cv_results)))
  expect_equal(cv_results$nridge, length(lambda_values))
  expect_equal(cv_results$lambda_grid, lambda_values)
  expect_true(is.numeric(cv_results$min_lambda))
  expect_equal(cv_results$m_folds, 10)
  expect_true(is.numeric(cv_results$MSPE))
  expect_equal(length(cv_results$MSPE), length(lambda_values))

  # Test case: Cross-validation with automatic lambda grid
  cv_results_auto <- regmodel(y ~ x1 + x2 + x3, data = as.data.frame(cbind(y,X)), model = "lasso", cv = T)

  expect_true(is.list(cv_results_auto))
  expect_true(all(c("nridge", "lambda_grid", "min_lambda", "m_folds", "MSPE") %in% names(cv_results_auto)))
  expect_true(cv_results_auto$nridge > 1)  # Check that more than one lambda was tested
  expect_true(is.numeric(cv_results_auto$min_lambda))
  expect_equal(cv_results_auto$m_folds, 10)
  expect_true(is.numeric(cv_results_auto$MSPE))
  expect_equal(length(cv_results_auto$MSPE), cv_results_auto$nridge)
})

