#' Returns the inflation adjusted future value of an single payment (fv).If fv is given as ts the deflated value of the singele paymetn is given for each year.
#'
#' @param fv The future value of an single payment (fv).
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper The total number of payment periods. Default is one period. If inflation is entered as ts nper is ignored.
#' @export
#' @examples
#' infladj.single(fv=-1000,inflation=0.02, nper= 25)



infladj.single <- function(fv,inflation=0, nper){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) {
    print("inflation must either be of type scalar or ts." )
  }else if(!is.scalar(nper)) {
    print("nper must be of type scalar" )
  }else{

  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = 1, start = c(1,1))
  }
  accInflation = ts(cumprod(1+inflation)-1, frequency = frequency(inflation), start = start(inflation))

  return(fv / (1+accInflation))
  }
}
