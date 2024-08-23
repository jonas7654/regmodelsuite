#' Check if a column of a data.frame is a dummmy variable
#' @param data data matrix


is_dummy <- function(data) {
  apply(data, 2, function(x) length(unique(x)) == 2)
}
