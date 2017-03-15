#' Returns the future value of an single payment (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param pv The present value of single payment made today. Default is assumed to be zero. Must be entered as a negative number
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv(rate=0.01,nper=10,pv=-1000)
#' fv.single(rate=0.01,nper=10,pv=-1000)

fv.single <- function(rate,inflation,nper,pv){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) {
    print("inflation must either be of type scalar or ts." )
  }else if(!(is.ts(rate) || is.scalar(rate))) {
    print("rate must either be of type scalar or ts" )
  }else if(!is.scalar(nper)) {
    print("nper must be of type scalar" )
  }else if(!is.scalar(pv)) {
    print("pv must be of type scalar" )
  }else{

  if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = 1, start = c(1,1))
  }
  accRate = ts(cumprod(rate+1)-1, frequency = frequency(rate), start = start(rate))

  fv = -pv*(1+accRate) ## non inflation adjusted

  if (any(inflation!=0)){
    fv = infladj.single(fv,inflation,nper)
  }
  return(fv)
  }
}
