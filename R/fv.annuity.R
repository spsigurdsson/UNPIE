#' Returns the future value of an annuity payment (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv(rate=0.01,nper=10,pmt=10,pmtUltimo=TRUE)

fv.annuity <- function(rate,nper,pmt,pmtUltimo=TRUE) {
  if(typeof(pmtUltimo)!= "logical"){
    print("pmtUltimo must be boolian" )
  }else{
    if(isTRUE(pmtUltimo)){adjustment=0 }else{adjustment=1}
    fv = (pmt / rate * ((1 + rate)^nper - 1))*(1+rate)^adjustment * (-1)
    return(fv)
  }
}
