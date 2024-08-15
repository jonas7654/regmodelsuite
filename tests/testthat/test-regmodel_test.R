#############

# use devtools::test()

#########

# Write tests

test_that("regmodel input checks work", {
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("X", 1:p)
  x1 <- X[ , 1]


  y <- rnorm(n)
  data <- data.frame(y, X1 = X[, 1], X2 = X[, 2], X3 = X[, 3], X4 = X[, 4], X5 = X[, 5])

  formula <- y ~ X1 + X2 + X3 + X4 + X5
  lambda <- 1


  # Test for ridge regression with a negative lambda
  expect_error(regmodel(formula = formula, data = data, model = "ridge", lambda = -1),
               "Lambda must be a positive number")
  # Test for ridge regression with negative lambda grid
  expect_error(regmodel(formula = formula, data = data, model = "ridge", lambda = c(-1,-2,-5,2)),
               "Lambda must be a positive number")
  # Test for ridge regression with a negative lambda
  expect_error(regmodel(formula = formula, data = data, model = "lasso", lambda = -1),
               "Lambda must be a positive number")
  # Test for ridge regression with negative lambda grid
  expect_error(regmodel(formula = formula, data = data, model = "lasso", lambda = c(-1,-2,-5,2)),
               "Lambda must be a positive number")

  # Test for wrong lambda
  expect_error(regmodel(formula = formula, data = data, model = "lasso", lambda = "1"),
               "Lambda must be a positive number")

  # Wrong model input
  expect_error(regmodel(formula = formula, data = data, model = lasso, lambda = c(-1,-2,-5,2)),
               "Please select a valid model")
  expect_error(regmodel(formula = formula, data = data, model = "Lasso", lambda = c(-1,-2,-5,2)),
               "Please select a valid model")

  # Formula input test
  expect_error(regmodel(formula = "y ~ x", data = data, model = "lasso", lambda = 1),
              "please provide a valid formula object")
  expect_error(regmodel(formula = as.formula(y ~ k) , data = data, model = "lasso", lambda = 1))

  # Find variables in environment

  ###
  # Das hier funktioniert nicht, da die x1 nicht gefunden wird in dieser testumgebung

  # expect_no_error(regmodel(formula = y ~ x1 , model = "lasso", lambda = 1))
  ###
  expect_error(regmodel(formula = y ~ . , model = "lasso", lambda = 1))


  # cv parameter test
  expect_error(regmodel(formula , data = data , model = "ridge", lambda = c(1, -2), cv = T))
  expect_error(regmodel(formula , data = data , model = "ridge", lambda = c(1, -2), cv = c(T,F)))
  expect_error(regmodel(formula , data = data , model = "ridge", lambda = c(1, -2), cv = 20))
  expect_error(regmodel(formula , data = data , model = "ridge", lambda = c(1, -2), cv = "TRUE"))

  expect_warning(regmodel(formula , data = data , model = "lasso", cv = T, lambda = 1))
  expect_no_warning(regmodel(formula , data = data , model = "lasso", cv = T, lambda = c(1,2,3)))

  # Lambda should be ignored
  expect_no_error(regmodel(formula , data = data ,model = "backward", lambda = c(1, -2), cv = T))
  expect_no_error(regmodel(formula , data = data ,model = "forward", lambda = c(1, -2), cv = T))
  expect_no_error(regmodel(formula , data = data ,model = "LAR", lambda = c(1, -2), cv = T))

})



test_that("missing data is handled", {
  n <- 100
  df_with_na_predictors <- data.frame(
    x1 = 1:n,
    x2 = rnorm(n),
    categorical = as.character(sample(c("A", "B", "C"), size = n, replace = TRUE)),
    y = rnorm(n)
  )

  # Introduce NA values in predictors
  df_with_na_predictors[1:10, "x1"] <- NA
  df_with_na_predictors[20:30, "x2"] <- NA

  # Define formula
  formula <- as.formula(y ~ x1 + x2 + as.factor(categorical))

  # Test for error due to missing values
  expect_error(regmodel(formula, data = df_with_na_predictors, model = "lasso", lambda = 0.5, cv = FALSE))

})



