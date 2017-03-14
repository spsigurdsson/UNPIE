#' Returns the adjusted future value of an investment (fv) for inflation
#'
#' @param fv The future value of an single payment (fv). Must be entered as a negative number
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @export
#' @examples
#' infladj(fv=-1000,inflation=0.02, nper= 25)

infladj <- function(fv,inflation=0, nper){
  return(-fv * (1 + inflation)^(-1*nper))
}
