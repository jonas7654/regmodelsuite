
print.ridge <- function(ridgeobj) {
  cat("Ridge Regression Results:\n")
  cat("Lambda: ", ridgeobj$lambda, "\n", sep = "")
  cat("Coefficients:\n")
  print(ridgeobj$coefficients)
}
