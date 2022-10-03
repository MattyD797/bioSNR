
#' Average Source Level
#'
#' This function calculates the average source level given a series of pressures measured at r distance from the source
#'
#' @param press Measured pressure values (prms, Pa)
#' Source-receiver distance (m)
#'    @param r if it is a constant
#'    @param changingR Boolean for if the distance in which we are measuring the sound changes
#' @param ref The pressure reference value e.g. 20*10^(-6) in the air, 10^(-6) underwater.
#' @param PL The propogation loss measured for the frequency of the call at the location of the recording system
#' @param boolR Boolean to print or the value result as a string. Should be true for HW problems. 
#' @return The average source level in dB re. ref Pa.
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
