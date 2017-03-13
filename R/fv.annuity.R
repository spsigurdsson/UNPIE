fv.annuity <- function(r,n,pmt,pmtPrimo=TRUE) {
  if(typeof(pmtPrimo)== "logical"){
    print("pmtPrimo must be boolian" )
  }else{
    if(isTRUE(pmtPrimo)){adjustment=0 }else{adjustment=1}
    fv = (pmt / r * ((1 + r)^n - 1))*(1+r)^adjustment * (-1)
    return(fv)
  }
}
