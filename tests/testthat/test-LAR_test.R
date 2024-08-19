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

test_that("LAR works", {

  # Test with p > n
  LAR_fit <- regmodel(formula, data, model = "LAR", lambda = 1, cv = F)
  var_names_from_model_object <- colnames(LAR_fit$coefficients)

  # Output should not contain any NA
  expect_false(anyNA(LAR_fit, recursive = T))

  expect_s3_class(LAR_fit, "LAR")
  expect_setequal(var_names_from_model_object, colnames(X))

  expect_equal(LAR_fit$mean_y, mean(y))
  expect_equal(LAR_fit$mean_x, colMeans(X))
  expect_equal(LAR_fit$sd_x, apply(X,2 ,function(x) {sd(x)}))

  # test coef
  coef_test_LAR <- coef(LAR_fit)

  expect_equal(coef_test_LAR, LAR_fit$coefficients)

  # Test predict
  # back to original scale
  manual_predict <- as.vector(mean(y) + as.matrix(scale(X)) %*% coef_test_LAR)

  expect_equal(manual_predict, predict(LAR_fit))

  # Test with new data
  X_newdata <- matrix(rnorm(n * p), nrow = n, ncol = p) |> as.data.frame()
  X_newdata_scaled <-  as.matrix(scale(X_newdata,
                                       center = colMeans(X),
                                       scale = apply(X,2 ,function(x) {sd(x)}))
  )


  manual_predict_new_data <- as.vector(mean(y) + X_newdata_scaled %*% coef_test_LAR)
  expect_equal(manual_predict_new_data, predict(LAR_fit, X_newdata))

  # This should give an error since dimension do not match
  X_newdata_fail1 <- matrix(rnorm(n * (p + 1)), nrow = n, ncol = p + 1) |> as.data.frame()
  X_newdata_fail2 <- matrix(rnorm(n * (p - 1)), nrow = n, ncol = p - 1) |> as.data.frame()

  expect_error(predict(LAR_fit, X_newdata_fail1))
  expect_error(predict(LAR_fit, X_newdata_fail2))


})
