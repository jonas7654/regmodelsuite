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
plot.lasso <- function(lassoobj, predictor) {
  if (!inherits(lassoobj, "lasso")) {
    #stop("The provided object is not of class 'lasso'")
    ta <- 1
  }

  t <- predictor
  X <- lassoobj$model[,t] * lassoobj$sd_x[t] + lassoobj$mean_x[t]
  y <- lassoobj$y

  df_data <- data.frame(x1 = X, y = y)

  poly <- get_polynomial(X,
                         target = t,
                         coeff = lassoobj$coefficients,
                         mean = lassoobj$mean_x[t],
                         sd = lassoobj$sd_x[t],
                         cols = colnames(lassoobj$model),
                         means = lassoobj$mean_x,
                         sds = lassoobj$sd_x)

  df_regression <- data.frame(x = poly$x,
                              y = poly$y + lassoobj$mean_y)

  # Suppress warnings because ylim cuts off values but we do not care about
  # those values, as we mainly only need all values of y and x in the plot
  suppressWarnings(print(
    ggplot(df_data, aes(x = X, y = y)) +
      geom_point(color = "darkslategray3", size = 1) +
      labs(title = "Lasso-Schaetzer", x = colnames(lassoobj$model)[t], y = "y") +
      geom_line(data = df_regression, aes(x = x, y = y), color = "deeppink2") +
      ylim(min(y) - 0.15 * (max(y)-min(y)), max(y) + 0.15 * (max(y)-min(y))) +
      xlim(min(X) - 0.07 * (max(X)-min(X)), max(X) + 0.07 * (max(X)-min(X))) +
      theme_minimal()
  ))
}
