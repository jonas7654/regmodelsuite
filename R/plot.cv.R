#' Plot the profile lines of cross validation
#'
#' Pass in a ridge object and select a predictor. The plot contains the original
#' points of the model and draw the estimation of y based solely on the selected
#' predictors. \cr
#' plot.ridge can plot real polynomials, so a model that was fitted for the formula
#' \code{y ~ x1 + x1^2 + 0.2*x1^5 + x2} could be plotted along x1.
#'
#' See \code{vignette("plot", package = "regmodel")} for detailed worked examples.
#'
#' @param cvobj A cv object
#'
#' @export
plot.cv <- function(cvobj) {

  if (!inherits(cvobj, "cv")) {
    stop("The provided object is not of class 'cv'")
  }

  lambda_seq <- cvobj$lambda_grid

  coeff_seq <- coef(cvobj, unscale = TRUE)

  if (inherits(cvobj,"ridge_cv")) {
    name <- "Ridge-Schätzer: Profillinien der Parameter"
  }
  else if (inherits(cvobj,"lasso_cv")) {
    name <- "LASSO-Schätzer: Profillinien der Parameter"
  }

  matplot(lambda_seq, t(coeff_seq),
          type = "l", lty = 1, col = 1:ncol(coeff_seq),
          xlab = "lambda", ylab = "geschätzte Parameter beta",
          main = name, log = "x"
  )

  legend("topright", legend = 1:nrow(coeff_seq),col = 1:nrow(coeff_seq),
         cex=0.8,fill=1:nrow(coeff_seq))

  #axis(1, at = c(1e-3, 1e-1, 1e1, 1e3), labels = c("1e-3", "1e-1", "1e1", "1e3"))

  abline(v = cvobj$min_lambda, col = "black", lty = 2)
}
