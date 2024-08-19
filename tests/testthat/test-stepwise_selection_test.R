X <- matrix(1:100, nrow = 20)
y <- 1:10
data <- data.frame(X, y)
formula <- as.formula(y ~ .)

test_that("forward selection input checks work", {
  expect_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = -1, verbose = F))

  expect_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 0, verbose = F))

  expect_no_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 1, verbose = F))
})


test_that("backward selection input checks work", {
  expect_error(regmodel(formula = formula, data = data, model = "backward",
                        n_predictors = -1, verbose = F))

  expect_error(regmodel(formula = formula, data = data, model = "backward",
                        n_predictors = 0, verbose = F))

  expect_error(regmodel(formula = formula, data = data.frame(x1=1, x2=1, y=1),
                        model = "backward", n_predictors = 1, verbose = F))

  expect_no_error(regmodel(formula = formula, data = data, model = "backward",
                            n_predictors = 1, verbose = F))
})

test_that("forward selection output is correct", {
  result <- regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 1, verbose = F)

  expect_true(round(result$error, 6) == 6.203008)
  #expect_true(result$predictors == "X2")
  expect_true(result$direction == "forward")
})

test_that("backward selection output is correct", {
  result <- regmodel(formula = formula, data = data, model = "backward",
                     n_predictors = 1, verbose = F)

  expect_true(round(result$error, 6) == 6.203008)
  expect_true(result$predictors == "X5")
  expect_true(result$direction == "backward")
})
