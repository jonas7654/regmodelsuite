#' Recursively search parent environments for data
#'
#'
#' @param data Name of data object
#' @param env current environment for recursive search
#'

recursive_data_search <- function(data, env) {

  if (exists(data, env)) {
    cat("Variable", data, "retrieved from: ")
    print(env)
    cat("\n")
    return(get(data, envir = env))
  }

  if (identical(env, .GlobalEnv)) {
    stop(paste("Variable", data, "not found in any parent environment"))
  }

  recursive_data_search(data, parent.env(env))
}
