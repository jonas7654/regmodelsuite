
#' @export

print.LAR <- function(x, ...) {
  cat("Least Angle Regression (LAR) Object\n")
  cat("-----------------------------------\n")
  cat("Number of predictors:", ncol(x$coefficients), "\n")
  cat("Number of steps:", nrow(x$coefficients), "\n\n")

  cat("Final coefficients (after full LAR path):\n")
  print(round(x$coefficients[nrow(x$coefficients), ], 4))

  cat("\nL1 Arc Lengths:\n")
  print(round(x$l1_arc_length, 4))

  if (!is.null(x$R2)) {
    cat("\nR-squared values at each step:\n")
    print(round(x$R2, 4))
  }

  cat("\nUse plot() to visualize the coefficient paths.\n")
}

