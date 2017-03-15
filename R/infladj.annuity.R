#' Returns the inflation adjusted future value of a cashflow with annuity payments.
#'
#' @param fv The future value of an single payment (fv).
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @export
#' @examples
#' infladj(fv=-1000,inflation=0.02, nper= 25)

infladj.annuity <- function(fv,rate=0, inflation=0, nper){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) {
    print("inflation must either be of type scalar or ts." )
  }else if(!(is.ts(rate) || is.scalar(rate))) {
      print("rate must either be of type scalar or ts." )
  }else if(!is.scalar(nper)) {
    print("nper must be of type scalar" )
  }else{

    if(is.scalar(rate)){
      rate = ts(rep(rate,nper), frequency = 1, start = c(1,1))
    }
    if(is.scalar(inflation)){
      inflation = ts(rep(inflation,nper), frequency = 1, start = c(1,1))
    }
    accRate = ts(cumprod(1+rate)-1, frequency = frequency(rate), start = start(rate))
    accInflation = ts(cumprod(1+inflation)-1, frequency = frequency(inflation), start = start(inflation))
    return(fv * (1 + accInflation)/(1+accRate))
  }
}
