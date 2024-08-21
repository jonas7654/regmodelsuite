#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ylim
#'
#' @export

# Helper function
get_polynomial <- function(X, target, coeff, mean, sd, cols, means, sds) {
  cols_i <- grep(cols[target], cols)
  browser()

  cols_t <- cols[cols_i]
  coeff_t <- coeff[cols_i]

  x <- seq(from = min(X), to = max(X), by = (max(X)-min(X))/100)
  # x <- scale(x, center = mean, scale = sd)
  y <- rep(0, length(x))

  for (i in 1:length(cols_t)) {
    if (grepl(paste0("I\\(", cols[target], "\\^[1-9]+\\)"), cols_t[i])) {
      # exponent expression, extract exponent
      exponent <- as.numeric(regmatches(cols_t[i],
                                        regexpr(paste0("(?<=I\\(", cols[target], "\\^)[1-9]+"),
                                                cols_t[i],
                                                perl = TRUE)))
      #
      x_temp <- scale(x, center = means[cols_i[i]], scale = sds[cols_i[i]])

      #
      y <- y + coeff_t[i] * x_temp^exponent

      # y <- y + coeff_t[i] * x^exponent
    }
    else if (cols[target] == cols_t[i]) {
      x_temp <- scale(x, center = means[cols_i[i]], scale = sds[cols_i[i]])

      #
      y <- y + coeff_t[i] * x_temp

      # y <- y + coeff_t[i] * x
    }
  }
  f(x) = b1 * x1 + b2*x2 + ....

  # need to add mean twice, because y and beta * x values are both demeaned.
  # y_t <- y * attr(y, "scaled:scale") +
  #  attr(y, "scaled:center")# + ridgeobj$mean_y

  return(list(x = x, y = y))
}

#' @export
plot.ridge <- function(ridgeobj, predictor) {

  if (!inherits(ridgeobj, "ridge")) {
    stop("The provided object is not of class 'ridge'")
  }

  t <- predictor
  X <- ridgeobj$model[,t] * ridgeobj$sd_x[t] + ridgeobj$mean_x[t]
  y <- ridgeobj$y

  df_data <- data.frame(x1 = X, y = y)

  poly <- get_polynomial(X,
                         target = t,
                         coeff = ridgeobj$coefficients,
                         mean = ridgeobj$mean_x[t],
                         sd = ridgeobj$sd_x[t],
                         cols = colnames(ridgeobj$model),
                         means = ridgeobj$mean_x,
                         sds = ridgeobj$sd_x)

  # need to add mean twice, because y and beta * x values are both demeaned.
  y_t <- poly$y * attr(poly$y, "scaled:scale") +
    attr(poly$y, "scaled:center") + ridgeobj$mean_y

  x_t <- poly$x * ridgeobj$sd_x[t] + ridgeobj$mean_x[t]

  df_regression <- data.frame(x = x_t, y = y_t)

  # Suppress warnings because ylim cuts off values but we do not care about
  # those values, as we mainly only need all values of y and x in the plot
  suppressWarnings(print(
    ggplot(df_data, aes(x = X, y = y)) +
      geom_point(color = "darkslategray3", size = 1) +
      labs(title = "Ridge-Schaetzer", x = colnames(ridgeobj$model)[t], y = "y") +
      geom_line(data = df_regression, aes(x = x, y = y), color = "deeppink2") +
      ylim(min(y) - 0.15 * (max(y)-min(y)), max(y) + 0.15 * (max(y)-min(y))) +
      theme_minimal()
  ))
}


#plot.ridge <- function(ridgeobj) {
#  if (!inherits(ridgeobj, "ridge")) {
#    stop("The provided object is not of class 'ridge'")
#  }

#  plot_values <- data.frame(x1 = sort(ridgeobj$model[,1]), y = ridgeobj$y, fitted = ridgeobj$coefficients)

#  slope <- ridgeobj$coefficients[1]

#  ggplot(plot_values, aes(x = x1, y = y)) +
#    geom_point(color = "blue", size = 3) +                # Werte als Punkte
#    geom_smooth(aes(y = fitted), color = "red", size = 1, span = 0.1, method = 'loess', formula = 'y ~ x', se = FALSE) + # Ridge-Schätzer Linie
#    labs(title = "Ridge-Schaetzer", x = "x1", y = "y") +   # Achsenbeschriftung und Titel

#    theme_minimal()

#}
