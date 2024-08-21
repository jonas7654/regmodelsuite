X <- matrix(1:100, nrow = 20)
y <- 1:10
data <- data.frame(X, y)
formula <- as.formula(y ~ .)

test_that("forward selection input checks work", {
  expect_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = -1))

  expect_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 0))

  expect_no_error(regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 1))
})


test_that("backward selection input checks work", {
  expect_error(regmodel(formula = formula, data = data, model = "backward",
                        n_predictors = -1))

  expect_error(regmodel(formula = formula, data = data, model = "backward",
                        n_predictors = 0))

  expect_error(regmodel(formula = formula, data = data.frame(x1=1, x2=1, y=1),
                        model = "backward", n_predictors = 1))

  expect_no_error(regmodel(formula = formula, data = data, model = "backward",
                            n_predictors = 1))
})

test_that("forward selection output is correct", {
  result <- regmodel(formula = formula, data = data, model = "forward",
                        n_predictors = 1)

  expect_true(round(result$error, 6) == 6.203008)
  #expect_true(result$predictors == "X2")
  expect_true(result$direction == "forward")
})

test_that("backward selection output is correct", {

  result <- regmodel(formula = formula, data = data, model = "backward",
                     n_predictors = 1)

  expect_true(round(result$error, 6) == 6.203008)
  expect_true(result$predictors == "X5")
  expect_true(result$direction == "backward")
})

test_that("predict function is working", {
  regmodel_fit_forward <- regmodel(mpg ~ factor(cyl, levels = c(8,6,4)),
                                   data = mtcars,
                                   model = "forward",
                                   n_predictors = 2)

  regmodel_fit_backward <- regmodel(mpg ~ factor(cyl, levels = c(8,6,4)),
                                   data = mtcars,
                                   model = "backward",
                                   n_predictors = 2)

  lm_fit <- lm(mpg ~ factor(cyl, levels = c(6,4,8)),
               data = mtcars)

  expect_false(anyNA(regmodel_fit_forward, recursive = T))
  expect_false(anyNA(regmodel_fit_backward, recursive = T))
  expect_false(anyNA(lm_fit, recursive = T))

  # Get coefficients
  coef_forward <- coef(regmodel_fit_forward)
  coef_backward <- coef(regmodel_fit_backward)
  coef_lm <- coef(lm_fit)

  # predictions
  forward_pred <- predict(regmodel_fit_forward)
  backward_pred <- predict(regmodel_fit_backward)
  lm_pred <- predict(lm_fit)

  expect_equal(forward_pred, lm_pred)
  expect_equal(backward_pred, lm_pred)
})
