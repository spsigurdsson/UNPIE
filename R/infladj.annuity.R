#' Returns the inflation adjusted future value of cashflow with (constant) annuity payments.
#'
#' @param fv The future value of an single payment (fv).
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param inflation The inflation rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @export
#' @examples
#' infladj.annuity(fv=-1000,rate=.06, inflation=0.02, nper=25)

infladj.annuity <- function(fv=1,rate=0, inflation=0, nper=1){
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.",call. = FALSE))
  if(!(is.ts(rate) || is.scalar(rate))) return(stop("rate must either be of type scalar or ts.",call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(is.ts(inflation) && is.ts(rate) && start(inflation) != start(rate)) return(stop("inflation and rate ts objects must have same start",call. = FALSE))

  #Find start, end and frequency
  if(is.ts(inflation)){
    start = start(inflation)
    end = end(inflation)
    frequency = frequency(inflation)
  }else if(is.ts(rate)) {
    start = start(rate)
    end = end(rate)
    frequency = frequency(rate)
  }else if(is.ts(fv)) {
    start = start(fv)
    end = end(fv)
    frequency = frequency(fv)
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

    realRate = rate.real(rate,inflation)
    accRealRate = ts(cumprod(1+realRate)-1, frequency = frequency(inflation), start = start, end = end)
    return(fv/(1+accRealRate))
}
