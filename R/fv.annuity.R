#' Returns the future value of annuity payments (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtinfladj Are the payments inflation adjusted? Default value = FALSE.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv.annuity(rate=0.01,nper=10,pmt=-10,pmtUltimo=TRUE)

fv.annuity <- function(rate,inflation,nper,pmt,pmtinfladj=FALSE,pmtUltimo=TRUE) {
  ##Type check
  if(!(is.ts(inflation) || is.scalar(inflation))) {
    print("inflation must either be of type scalar or ts." )
  }else if(!(is.ts(rate) || is.scalar(rate))) {
    print("rate must either be of type scalar or ts" )
  }else if(!is.scalar(nper)) {
    print("nper must be of type scalar" )
  }else if(!(is.ts(pmt) || is.scalar(pmt))) {
    print("pmt must either be of type scalar or ts" )
  }else if(typeof(pmtUltimo)!= "logical"){
    print("pmtUltimo must be boolian" )
  }else if(typeof(pmtinfladj)!= "logical"){
    print("pmtinfladj must be boolian" )
  }else{

    if(isTRUE(pmtUltimo)){
      adjustment=0
    }else{adjustment=1
    }

    if(is.scalar(rate)){
    rate = ts(rep(rate,nper), frequency = 1, start = c(1,1))
    }
    if(is.scalar(pmt)){
      pmt = ts(rep(pmt,nper), frequency = 1, start = c(1,1))
    }

    accRate = ts(cumprod(rate+1)-1, frequency = frequency(rate), start = start(rate))
    fv = -pmt/rate * accRate *(1+rate)^adjustment

    if (any(inflation!=0)){
      fv = infladj.annuity(fv,rate,inflation,nper)
    }
    return(fv)
  }
}
