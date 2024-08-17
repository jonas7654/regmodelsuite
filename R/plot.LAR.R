#' @export

plot.LAR <- function(lar_obj) {
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
