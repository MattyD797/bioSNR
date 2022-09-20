
#' Average Source Level
#'
#' This function calculates the average source level given a series of pressures
#'
#' @param press Measured pressure values
#' @param changingR Boolean for if the distance in which we are measuring the sound change
#' @param ref The reference value
#' @param r The distance of the source if constant
#' @param PL The propogation loss measured from our instrument
#' @param boolR Boolean of whether you want the value printed out in a string. Should be true for HW problems. 
#' @return The average source level in dB rel to pressure references
#' @export
avgSL <- function(press, changingR = F, ref = 20*10^(-6), r = 50, PL = 20, 
                  boolR = T){
  
  if(is.character(press)){
    dt <- read.csv(file, header =F)
    prms <- dt[,1]
    if(changingR){
      r <- dt[,2]
      
      SL_series = 20*log10(prms/pref) + PL*log10(r)
      
      SL_social = 20*log10(mean(10^(SL_series/20)))
      
      if(boolR){
        print(paste("The source level (SL) of the long-range call, obtained using spherical spreading is", SL_social, "dB"))
      }
      
      return(SL_social)
    }
    
    
  } else {
    dt <- press
    SL1 <- dt + PL*log10(r)
    
    if(boolR){
      print(paste("The source level (SL) of the long-range call, obtained using spherical spreading is", SL1, "dB"))
    }
    return(SL1)
  }
  
  #To estimate  at 50 m, we can average the  measurements  and convert the average value in dB
  RL1 = 20*log10(mean(prms)/ref)
  
  #We can now retreive the SL in dB re. μPa, compensating for the spherical transmission losses between the 
  #source and the receiver 
  SL1 = RL1 + PL*log10(r)
  
  if(boolR){
    print(paste("The source level (SL) of the long-range call, obtained using spherical spreading is", SL1, "dB"))
  }
  return(SL1)
}
