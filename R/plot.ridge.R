#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 xlim

# Helper function
get_polynomial <- function(X, target, coeff, mean, sd, cols, means, sds) {
  cols_i <- grep(cols[target], cols)
  cols_t <- cols[cols_i]
  coeff_t <- coeff[cols_i]

  x <- seq(from = min(X), to = max(X), by = (max(X)-min(X))/100)
  y <- rep(0, length(x))

  for (i in 1:length(cols_t)) {
    if (grepl(paste0("I\\([\\+\\-]?", cols[target], "\\^[1-9]+\\)"), cols_t[i])) {
      # exponent expression, extract exponent
      exponent <- as.numeric(regmatches(cols_t[i],
                                        regexpr(paste0("(?<=I\\([\\+\\-]?", cols[target], "\\^)[1-9]+"),
                                                cols_t[i],
                                                perl = TRUE)))
      y <- y + (coeff_t[i]/sds[cols_i[i]]) * (x**exponent-means[cols_i[i]])
    }
    else if (cols[target] == cols_t[i]) {
      y <- y + coeff_t[i] * ((x-means[cols_i[i]])/sds[cols_i[i]])
    }
  }
  return(list(x = x, y = y))
}


#' @export
plot.ridge <- function(ridgeobj, predictor) {

  if (!inherits(ridgeobj, "ridge")) {
    stop("The provided object is not of class 'ridge'")
    ta <- 1
  }

  if (is.character(predictor)) {
    predictor <- which(colnames(ridgeobj$model) == predictor)
    if (length(predictor) == 0) {
      stop("Could not find predictor")
    }
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

  df_regression <- data.frame(x = poly$x,
                              y = poly$y + ridgeobj$mean_y)

  # Suppress warnings because ylim cuts off values but we do not care about
  # those values, as we mainly only need all values of y and x in the plot
  suppressWarnings(print(
    ggplot(df_data, aes(x = X, y = y)) +
      geom_point(color = "darkslategray3", size = 1) +
      labs(title = "Ridge-Schaetzer", x = colnames(ridgeobj$model)[t], y = "y") +
      geom_line(data = df_regression, aes(x = x, y = y), color = "deeppink2") +
      ylim(min(y) - 0.15 * (max(y)-min(y)), max(y) + 0.15 * (max(y)-min(y))) +
      xlim(min(X) - 0.07 * (max(X)-min(X)), max(X) + 0.07 * (max(X)-min(X))) +
      theme_minimal()
  ))
}
