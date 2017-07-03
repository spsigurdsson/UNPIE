

#* Add a car to the inventory
#* @get /fv
#* @param rate:numeric The interest rate per period. Default is zero. Must be entered as decimal.
#* @param inflation:numeric The inflation forcast. Default is zero. Must be entered as decimal.
#* @param nper:int The total number of payment periods. Default is one period.
#* @param pv:numeric The present value of single investment made today. Default is assumed to be zero. Must be entered as a negative number.
#* @param pmt:numeric The payment made each period (annuity). Must be entered as a negative number.
#* @param pmtinfladj:logical Should the payments be inflation adjusted? E.g. are the annuity pmt constant or real annuities. Only avaliable for pmt given as scalar. Default value = FALSE.
#* @param pmtUltimo:logical When payments are due. TRUE = end of period, FALSE = beginning of period. Default is TRUE.
function(rate=0,inflation=0, nper=1,pv=0,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE){
  unpie::fv(
    rate = as.numeric(rate),
    inflation = as.numeric(inflation),
    nper = as.numeric(nper),
    pv = as.numeric(pv),
    pmt = as.numeric(pmt),
    pmtinfladj = as.logical(pmtinfladj),
    pmtUltimo = as.logical(pmtUltimo)
  )

}
