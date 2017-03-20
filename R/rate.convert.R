#' Converts an rate with a given frequency to aneffective rate for another frequence. Base=1 is yearly rate
#'
#' @param rate interest rate.
#' @param frequency.input Frequency of input rate. 1=annual, 12=monthly.
#' @param frequency.output Frequency of output rate. 1=annual, 12=monthly.
#' @export
#' @examples
#' rate.convert(rate=0.00327374,frequency.input=12,frequency.output=1) #Monthly to annual Rate
#' rate.convert(rate=0.04,frequency.input=1,frequency.output=12) #Annual to monthly Rate

rate.convert <- function(rate=0,frequency.input=1,frequency.output=12){

  ##Type check
  if(!is.scalar(rate))return(stop("rate must be of type scalar",call. = FALSE))

  return((1+rate)^(frequency.input/frequency.output)-1)
}
