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

  expect_equal(round(result$error, 6), 6.203008)
  expect_equal(length(result$predictors), 1)
  expect_equal(result$direction, "forward")
})

test_that("backward selection output is correct", {

  result <- regmodel(formula = formula, data = data, model = "backward",
                     n_predictors = 1, verbose = F)

  expect_equal(round(result$error, 6), 6.203008)
  expect_equal(length(result$predictors), 1)
  expect_equal(result$direction, "backward")
})

test_that("predict function is working", {
  formula <- mpg ~ factor(cyl, levels = c(8,6,4))

  regmodel_fit_forward <- regmodel(formula,
                                   data = mtcars,
                                   model = "forward",
                                   n_predictors = 2,
                                   verbose = F)
  regmodel_fit_backward <- regmodel(formula,
                                    data = mtcars,
                                    model = "backward",
                                    n_predictors = 2,
                                    verbose = F)
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


  # Test with other models
  data <- mtcars
  stepwise_ridge <- regmodel(formula,
                             data = data,
                             model = "forward",
                             n_predictors = 2,
                             verbose = F,
                             model_fct=\(formula, data) regmodel(formula, data,
                                                                 "ridge",
                                                                 lambda= 0))

  expect_false(anyNA(stepwise_ridge, recursive = T))
})
