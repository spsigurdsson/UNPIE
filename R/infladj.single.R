#' Returns the inflation adjusted future value of an single payment (fv).If fv is given as ts the deflated value of the singele paymetn is given for each year.
#'
#' @param fv The future value of an single payment (fv).
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper The total number of payment periods. Default is one period. If inflation is entered as ts nper is ignored.
#' @export
#' @examples
#' infladj.single(fv=-1000,inflation=0.02, nper= 25)



infladj.single <- function(fv=1,inflation=0, nper=1){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.",call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(is.ts(inflation) && start(fv) != start(inflation)) return(stop("inflation and fv ts objects must have same start",call. = FALSE))

  #Find start
  if(is.ts(inflation)){
    start = start(inflation)
  }else if(is.ts(fv)) {
    start = start(fv)
  }else{
    start = c(1,1)
  }
  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = 1, start)
  }
  accInflation = ts(cumprod(1+inflation)-1, frequency = frequency(inflation), start = start)
  return(fv / (1+accInflation))
}

