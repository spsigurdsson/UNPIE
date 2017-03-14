#' Returns the future value of an single payment (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param pv The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv(rate=0.01,nper=10,pv=-1000)
#' fv.single(rate=0.01,nper=10,pv=-1000)

fv.single <- function(rate,inflation,nper,pv){
  fv =(pv * (1 + rate)^nper)*(-1) ## non inflation adjusted
  if (inflation!=0){
    fv = infladj(-fv,inflation,nper)
  }
  return(fv)
}
