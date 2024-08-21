#' @export

plot.ridge <- function(ridgeobj) {
  if (!inherits(ridgeobj, "ridge")) {
    stop("The provided object is not of class 'ridge'")
  }

  plot_values <- data.frame(x1 = sort(ridgeobj$model[,1]),
                            y = ridgeobj$y, fitted = ridgeobj$coefficients)

  slope <- ridgeobj$coefficients[1]

  ggplot(plot_values, aes(x = x1, y = y)) +
    geom_point(color = "blue", size = 3) +                # Werte als Punkte
    geom_abline(slope = slope, intercept = 0) + # Ridge-Schätzer Linie
    labs(title = "Ridge-Schätzer", x = "x1", y = "y") +   # Achsenbeschriftung und Titel
    theme_minimal()

}
