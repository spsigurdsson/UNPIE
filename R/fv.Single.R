#' Estimate future value (fv) of a single sum
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pv present value
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv.single(0.08,10,-300)
#'
#' fv.single(r=0.04,n=20,pv=-50000)

fv.single <- function(rate,nper,pv){
  return((pv * (1 + rate)^nper)*(-1))
}
