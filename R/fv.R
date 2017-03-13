fv <- function(r,n,pv=0,pmt=0,pmtPrimo=TRUE){
  if(typeof(pmtPrimo)== "logical"){
    print("pmtPrimo must be boolian" )
  }else{
    if(isTRUE(pmtPrimo)){adjustment=0 }else{adjustment=1}
    return(fv.Single(r,n,pv) + fv.Annuity(r,n,pmt,pmtPrimo))
  }
}
