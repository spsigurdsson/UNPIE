#' Returns the future value of annuity payments (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj Are the payments inflation adjusted? Default value = FALSE.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv.annuity(rate=0.01,nper=10,pmt=-10,pmtUltimo=TRUE)

fv.annuity <- function(rate=0,inflation=0,nper=1,pmt=0,pmtinfladj=FALSE,pmtUltimo=TRUE) {
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.", call. = FALSE))
  if(!(is.ts(rate) || is.scalar(rate))) return(stop("rate must either be of type scalar or ts", call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(!(is.ts(pmt) || is.scalar(pmt))) return(stop("pmt must either be of type scalar or ts", call. = FALSE))
  if(typeof(pmtUltimo)!= "logical") return(stop("pmtUltimo must be boolian", call. = FALSE))
  if(typeof(pmtinfladj)!= "logical") return(stop("pmtinfladj must be boolian", call. = FALSE))
  if(is.ts(pmt) && is.ts(rate) && start(pmt) != start(rate)) return(stop("pmt and rate ts objects must have same start", call. = FALSE))

  if(isTRUE(pmtUltimo)){
    adjustment=0
  }else{
    adjustment=1
  }

  #Find start
  if(is.ts(pmt)){
    start = start(pmt)
    end = end(pmt)
  }else if(is.ts(rate)) {
    start = start(rate)
    end = end(rate)
  }else if(is.ts(inflation)) {
    start = start(inflation)
    end = end(inflation)
  }else{
    start = c(1,1)
    end = c(nper,1)
  }

  #Find frequency
  if(is.ts(pmt)){
    frequency = frequency(pmt)
  }else if(is.ts(rate)) {
    frequency = frequency(rate)
  }else if(is.ts(inflation)) {
    frequency = frequency(inflation)
  }else{
    frequency = 1
  }

  if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = frequency, start = start, end = end)
  }
  if(is.scalar(pmt)){
    pmt = ts(rep(pmt,nper), frequency = frequency, start = start,end=end)
  }
  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = frequency, start = start, end = end)
  }

  accRate = ts(cumprod(rate+1)-1, frequency = frequency, start = start, end = end)
  fv = -pmt/rate * accRate *(1+rate)^adjustment

  if (any(inflation!=0)){
    fv = infladj.annuity(fv,rate,inflation,nper)
  }
  return(fv)
}
