#' Plots the selection process of a stepwise selection object.
#'
#' @param object Stepwise selection object
#' @return A ggplot.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggtitle
#' @export
plot.stepwise_selection <- function(object) {
  plot_data <- data.frame(x = length(object$predictors):object$start_predictors,
                          y = object$errors)

  ggplot(plot_data, aes(x, y)) +
    xlab("Predictor Subset Size") +
    ylab("Error") +
    geom_point(size = 3) +
    geom_line() +
    theme_minimal() +
    ggtitle("Stepwise selection")
}
