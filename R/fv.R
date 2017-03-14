#' Returns the future value of an investment (fv)
#'
#' @param rate The interest rate per period. Default is zero. Must be entered as decimal
#' @param inflation The inflation forcast. Default is zero. Must be entered as decimal
#' @param nper The total number of payment periods. Default is one period
#' @param pv The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number
#' @param pmt The payment made each period (annuity). Must be entered as a negative number.
#' @param pmtUltimo When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
#' @seealso \code{\link{fv.single}}
#' @seealso \code{\link{fv.annuity}}
#' @export
#' @examples
#' fv(rate=0.04,inflation=0.02, nper=10,pv=-1000,pmt=-10,pmtUltimo=TRUE)
#' fv(rate=0.04,inflation=0.02,nper=10,pmt=-10,pmtUltimo=TRUE)
#' fv(rate=0.04,inflation=0.02,nper=10,pv=-1000)

fv <- function(rate=0,inflation=0, nper=1,pv=0,pmt=0,pmtUltimo=TRUE){
  if(typeof(pmtUltimo)!= "logical"){
    print("pmtUltimo must be boolian" )
  }else{
    if(isTRUE(pmtUltimo)){adjustment=0 }else{adjustment=1}
    return(fv.single(rate,inflation,nper,pv) + fv.annuity(rate,inflation,nper,pmt,pmtUltimo))
  }
}
