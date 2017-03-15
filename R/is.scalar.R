#' Tests if an input is a scalar. Returns a boolean.
#'
#' @param x Object to be tested
#' @export
#' @examples
#' is.scalar(0.04)
#' is.scalar(4)
#' is.scalar(c(1:3))

is.scalar <- function(x) is.atomic(x) && length(x) == 1L
