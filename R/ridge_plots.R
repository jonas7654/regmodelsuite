ridge_plots <- function(X,y,m){
  ridge_plot_profile_lines(X,y,m)
  ridge_plot(X,y,m)
}


get_polynomial2 <- function(from, to, by, coeff, mean_t, sd_t, cols, target_col) {

  # browser()

  cols_t <- grep(target_col, cols, value = TRUE)

  cols_i <- grep(target_col, cols)

  cols_t <- cols[cols_i]
  coeff_t <- coeff[cols_i]

  # Definieren der x-Werte (z.B. von -10 bis 10)
  x <- seq(from, to, by)

  x <- scale(x, center = mean_t, scale = sd_t)

  # Definieren des Polynoms (z.B. y = 2x^2 - 3x + 1)
  y <- 0*x

  for (i in 1:length(cols_t)) {

    y <- y + coeff_t[i] * x^i
  }

  return(list(x = x, y = y))
}

ridge_plot <- function(ridgeobj, predictor_var){
  # browser()
  # cv_line <- predict(ridgeobj)

  y <- ridgeobj$y

  t <- predictor_var
  X <- ridgeobj$model[,t] * ridgeobj$sd_x[t] + ridgeobj$mean_x[t]

  df <- data.frame(x1 = X, y = y)

  poly <- get_polynomial2(from = min(X),
                         to = max(X),
                         by = (max(X)-min(X))/100,
                         coeff = ridgeobj$coefficients,
                         mean_t = ridgeobj$mean_x[t],
                         sd_t = ridgeobj$sd_x[t],
                         colnames(ridgeobj$model),
                         colnames(ridgeobj$model)[t])

  y_t <- poly$y * attr(poly$y, "scaled:scale") +
    attr(poly$y, "scaled:center") + ridgeobj$mean_y
  # lines(X[order(X)], cv_line[order(X)], col = "blue", lwd = 2)

  x_t <- poly$x * ridgeobj$sd_x[t] + ridgeobj$mean_x[t]

  df_2 <- data.frame(x = x_t, y = y_t)

  bot <- min(X)
  top <- max(X)

  ggplot(df, aes(x = X, y = y)) +
    geom_point(color = "darkslategray3", size = 1) +
    labs(title = "Ridge-Schaetzer", x = colnames(ridgeobj$model)[t], y = "y") +
    geom_line(color = "deeppink2", data = df_2, aes(x = x, y = y)) +
    ylim(bot, top) +
    theme_minimal()

  # browser()

  # plot(X, y, main="Ridge-Schaetzer", xlab=colnames(ridgeobj$model)[t], ylab="y", pch=19,
  #      col="darkslategray3", ylim = c(min(y,0), max(y)), las = 1)
  # abline(ridgeobj$mean_y-ridgeobj$coefficients[t]*ridgeobj$mean_x[t],
  #       ridgeobj$coefficients[t], col = "red", lwd = 2)

  # poly <- get_polynomial(from = min(X),
  #               to = max(X),
  #               by = (max(X)-min(X))/100,
  #               coeff = ridgeobj$coefficients,
  #               mean_t = ridgeobj$mean_x[t],
  #               sd_t = ridgeobj$sd_x[t],
  #               colnames(ridgeobj$model),
  #               colnames(ridgeobj$model)[t])

  #y_t <- poly$y * attr(poly$y, "scaled:scale") +
  #  attr(poly$y, "scaled:center") + ridgeobj$mean_y
 # lines(X[order(X)], cv_line[order(X)], col = "blue", lwd = 2)

  #x_t <- poly$x * ridgeobj$sd_x[t] + ridgeobj$mean_x[t]


  #lines(x_t, y_t, col = "red", lwd = 2)

  # browser()

  # abline(ridgeobj$mean_y,
  #        0, col = "red")
  #  lines(sort(X), cv_line, col="red", lwd=2)
}

ridge_plot_profile_lines <- function(X,y,m){
  lambda_seq <- 10^seq(3, -3, by = -0.1)
  ridge_seq <- matrix(0, nrow = ncol(X), ncol = length(lambda_seq))
  for (i in seq_along(lambda_seq)) {
    lambda <- lambda_seq[i]
    ridge_seq[, i] <- ridge(X, y, lambda)
  }

  matplot(lambda_seq, t(ridge_seq),
          type = "l", lty = 1, col = 1:ncol(ridge_seq),
          xlab = "lambda", ylab = "geschätzte Parameter beta",
          main = "Ridge-Schätzer: Profillinien der Parameter", log = "x", xaxt = "n"
  )

  axis(1, at = c(1e-3, 1e-1, 1e1, 1e3), labels = c("1e-3", "1e-1", "1e1", "1e3"))

  abline(v = cross_validation(X,y,m)$cv.lambda, col = "black", lty = 2)
}
