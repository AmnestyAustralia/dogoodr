#' Concatenate and post to console
#'
#' Applies glue to the string it receives, and returns if silent=FALSE
#'
#' @param msg character with concatenations in curly brackets
#' @param silent whether to message
#' @param env environment with variables to concatenate
#'
#' @return a console log
#' @importFrom glue glue
polite_message <- function(msg, silent = TRUE, env = parent.frame()) {
  if (!silent) {
    return(message(glue(msg, .envir = env)))
  }
}
