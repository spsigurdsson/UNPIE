#' Converts an rate with a given frequency to aneffective rate for another frequence. Base=1 is yearly rate
#'
#' @param nominalRate nominal rate.
#' @param inflation inflation.
#' @export
#' @examples
#' rate.real(nominalRate=0.00327374,inflation=0.02)


rate.real<- function(nominalRate, inflation){

  realRate = (1+nominalRate)/(1+inflation)-1
  return(realRate)

}
