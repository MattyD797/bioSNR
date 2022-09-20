#' Active Space Formula
#'
#' This function calculates the active space (AS)
#'
#' @param SL Source Level measured from the field 
#' @param NL Noise Level gathered from wenz graph (specLvlGraph)
#' @param freq The  frequency of center frequency used in calculating the absorbance
#' @param r A vector of length two where the radius of our active space may lie
#' @param PL The propogation loss (default is 20). Gathered from measurements. 
#' @param boolR Boolean of whether you want the value printed out in a string. Should be true for HW problems. 
#' @return The active space in meters
#' @export
activeSpace <- function(SL,NL,freq,r=c(0,1000),PL=20, boolR = T){
  AS <- uniroot(recievedLevel, r, SL, NL, freq, PL)
  if(boolR){
    print(paste("The active space is", AS[[1]], "meters"))
  }
  return(AS[[1]])
}

#' Received Level Function
#'
#' This function calculates the recieved level of a source level given noise level and absorption
#'
#' @param r A vector of length two where the radius of our active space may lie
#' @param SL Source Level measured from the field 
#' @param NL Noise Level gathered from wenz graph (specLvlGraph)
#' @param freq The  frequency of center frequency used in calculating the absorbance
#' @param PL The propogation loss (default is 20). Gathered from measurements.
#' @return The received level
recievedLevel <- function(r, SL, NL, freq, PL){
  RL = (SL-PL*log10(r) - 0.01*freq^(1/3)*r) - NL
  return(RL)
}
