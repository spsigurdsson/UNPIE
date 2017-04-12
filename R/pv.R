#' Returns the present value of a single payment and annuity payments (spending) made in the future (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal.
#' @param inflation The inflation forcast. Default is zero. Must be entered as decimal.
#' @param nper The total number of payment periods. Default is one period.
#' @param fv The future value of single spending made in the future. Default is assumed to be zero. Must be entered as a negative number.
#' @param pmt The payment (spending) made each period (annuity) in the future. Must be entered as a negative number.
#' @param pmtinfladj Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv.single}}
#' @seealso \code{\link{fv.annuity}}
#' @import stats
#' @export
#' @examples
#' pv(rate=0.04,inflation=0.02, nper=10,fv=-1000,pmt=-10,pmtinfladj=FALSE,pmtUltimo=TRUE)
#' pv(rate=0.04,inflation=0.02,nper=10,pmt=-10,pmtinfladj=TRUE,pmtUltimo=TRUE)
#' pv(rate=0.04,inflation=0.02,nper=10,fv=-1000)

pv <- function(rate=0,inflation=0, nper=1,fv=0,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE){
  ##Type check
  if(typeof(pmtUltimo)!= "logical") return(stop("pmtUltimo must be boolian" ,call. = FALSE))
  if(typeof(pmtinfladj)!= "logical") return(stop("pmtinfladj must be boolian",call. = FALSE))
  if(!(is.ts(rate) || is.scalar(rate))) return(stop("rate must either be of type scalar or ts.",call. = FALSE))
  if(!(is.ts(inflation) || is.scalar(inflation))) return(stop("inflation must either be of type scalar or ts.",call. = FALSE))
  if(!(is.ts(pmt) || is.scalar(pmt))) return(stop("pmt must either be of type scalar or ts",call. = FALSE))
  if(!is.scalar(nper)) return(stop("nper must be of type scalar",call. = FALSE))
  if(nper<1) return(stop("nper must be larger than zero",call. = FALSE))
  if(!is.scalar(fv)) return(stop("fv must be of type scalar",call. = FALSE))


  if(isTRUE(pmtUltimo)){
    adjustment=0
  }else{
    adjustment=1
  }

  #Find start, end, frequency
  if(is.ts(pmt)){
    start = start(pmt)
    end = end(pmt)
    frequency = frequency(pmt)
  }else if(is.ts(rate)) {
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
    rate = ts(rep(rate,nper), frequency = frequency, start = start,end = end)
  }
  if(is.scalar(pmt)){
    pmt = ts(rep(pmt,nper), frequency = frequency, start = start, end = end)
  }
  if(is.scalar(inflation)){
    inflation = ts(rep(inflation,nper), frequency = frequency, start = start, end = end)
  }

  return(pv.single(rate,inflation,nper,fv)+pv.annuity(rate,inflation,nper,pmt,pmtinfladj,pmtUltimo))
}
