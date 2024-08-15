plot.ridge <- function(ridgeobj) {
  if (!inherits(ridgeobj, "ridge")) {
    stop("The provided object is not of class 'ridge'")
  }
  
  library(ggplot2)
  
  plot_values <- data.frame(x1 = sort(ridgeobj$model[,1]), y = ridgeobj$y, fitted = ridgeobj$coefficients)
  
  ggplot(plot_values, aes(x = x1, y = y)) +
    geom_point(color = "blue", size = 3) +                # Werte als Punkte
    geom_smooth(aes(y = fitted), color = "red", size = 1, span = 0.1, method = 'loess', formula = 'y ~ x', se = FALSE) + # Ridge-Schätzer Linie
    labs(title = "Ridge-Schätzer", x = "x1", y = "y") +   # Achsenbeschriftung und Titel
    theme_minimal()                                       
}



#test
r <- list()
class(r) <- "ridge"
set.seed(21) 
r$model <- matrix(runif(100 * 10), 100, 10) 
r$y <- X %*% runif(10, min = -1, max = 1) + rnorm(100, mean = 0, sd = 0.3) 
r$coefficients<-ridge(X,y,0.1)
plot.ridge(r)
