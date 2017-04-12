#' Returns the future value of an single payment (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal or ts
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal or ts
#' @param nper The total number of payment periods. Default is one period. If rate and inflation are entered as ts nper is ignored.
#' @param fv The present value of single payment made today. Default is assumed to be zero. Must be entered as a negative number
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' pv.single(rate=0.05,inflation=0.03,nper=35,fv=-1000)
#' pv.single(rate=0.01,inflation=0,nper=10,fv=-1000)

pv.single <- function(rate=0,inflation=0,nper=1,fv=0){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.",call. = FALSE))
  if(!(is.ts(rate) || is.scalar(rate))) return(stop("rate must either be of type scalar or ts",call. = FALSE))
  if(!is.scalar(nper)|| nper<1 ) return(stop("nper must be of type integer larger than zero",call. = FALSE))
  if(!is.scalar(fv)) return(stop("pv must be of type scalar",call. = FALSE))

  #Find start, end and frequency
  if(is.ts(rate)){
    start = start(rate)
    end = end(rate)
    frequency = frequency(rate)
  }else if(is.ts(inflation)) {
    start = start(inflation)
    end = end(inflation)
    frequency = frequency(inflation)
  }else{
    start = c(1,1)
    end = c(nper,1)
    frequency = 1
  }

  if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = 1, start = start, end = end)
  }
  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = 1, start = start, end = end)
  }

  accRate = ts(cumprod(rate+1)-1, frequency = frequency(rate), start = start, end = end)
  pv = -fv/(1+accRate) ## non inflation adjusted

  if (any(inflation!=0)){
    pv = infladj.single(pv,inflation,nper)
  }
  return(pv)
}
