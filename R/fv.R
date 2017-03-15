#' Returns the future value of a single payment and annuity payments (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal.
#' @param inflation The inflation forcast. Default is zero. Must be entered as decimal.
#' @param nper The total number of payment periods. Default is one period.
#' @param pv The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number.
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj Are the payments inflation adjusted? Default value = FALSE.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv.single}}
#' @seealso \code{\link{fv.annuity}}
#' @export
#' @examples
#' fv(rate=0.04,inflation=0.02, nper=10,pv=-1000,pmt=-10,pmtinfladj=FALSE, pmtUltimo=TRUE)
#' fv(rate=0.04,inflation=0.02,nper=10,pmt=-10,pmtinfladj=TRUE,pmtUltimo=TRUE)
#' fv(rate=0.04,inflation=0.02,nper=10,pv=-1000)

fv <- function(rate=0,inflation=0, nper=1,pv=0,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE){
  ##Type check
  if(typeof(pmtUltimo)!= "logical"){
    print("pmtUltimo must be boolian" )
  }else if(typeof(pmtinfladj)!= "logical"){
    print("pmtinfladj must be boolian" )
  }else if(!(is.ts(rate) || is.scalar(rate))) {
    print("rate must either be of type scalar or ts." )
  }else if(!(is.ts(inflation) || is.scalar(inflation))) {
    print("inflation must either be of type scalar or ts." )
  }else if(!(is.ts(pmt) || is.scalar(pmt))) {
    print("pmt must either be of type scalar or ts" )
  }else if(!is.scalar(nper)) {
    print("nper must be of type scalar" )
  }else if(!is.scalar(pv)) {
    print("pv must be of type scalar" )

  }else{

    if(isTRUE(pmtUltimo)){
      adjustment=0
    }else{
      adjustment=1
    }

    fv=0
    if(pv!=0)
    {
      fv=fv.single(rate,inflation,nper,pv)
    }
    if (any(pmt!=0)){
      fv= fv + fv.annuity(rate,inflation,nper,pmt,pmtinfladj,pmtUltimo)
    }
    return(fv)
  }
}
