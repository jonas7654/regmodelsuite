#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 aes
#'
#' @export

plot.LAR <- function(lar_obj) {

if (ncol(lar_obj$coefficients) > 50) {
  warning("The amount of variables is larger than 50. The Plot might not be readible")
}

coefficients_long <- as.data.frame(lar_obj$coefficients)
coefficients_long$L1_Arc_Length <- lar_obj$l1_arc_length

coefficients_melted <- tidyr::pivot_longer(coefficients_long,
                                           cols = -L1_Arc_Length,
                                           names_to = "variable",
                                           values_to = "value")

# Plot
ggplot(coefficients_melted, aes(x = L1_Arc_Length, y = value, color = variable)) +
  geom_line() +
  labs(x = expression(L[1]~Arc~Length), y = "Coefficients",
       title = "LAR Coefficient Paths") +
  theme_minimal() +
  theme(legend.title = element_blank())
}
