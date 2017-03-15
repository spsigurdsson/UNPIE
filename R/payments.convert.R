#' Returns the future value of annuity payments (fv)
#'
#' @seealso \code{\link{fv}}
#' @export
#' @examples
#' fv.annuity(rate=0.01,nper=10,pmt=-10,pmtUltimo=TRUE)

payment.convert <- function(inflation,nper,pmt,pmtNominal=FALSE) {
  if(typeof(pmtNominal)!= "logical"){
    print("pmtNominal must be boolian" )

  }else{
    return(0)
  }
}
