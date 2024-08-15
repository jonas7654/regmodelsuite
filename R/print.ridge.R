
print.ridge <- function(ridgeobj) {
  if (!inherits(ridgeobj, "ridge")) {
    stop("The provided object is not of class 'ridge'")
  }

  cat("Call: ")
  print(ridgeobj$call)
  cat("\n")
  cat("Ridge Regression Results:\n")
  cat("Lambda: ", ridgeobj$lambda, "\n", sep = " ")
  cat("Coefficients:\n")
  print(ridgeobj$coefficients)
}
