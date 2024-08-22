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
