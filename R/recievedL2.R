#' Received Level
#'
#' This function calculates the received level between two individuals at a particular distance.
#'
#' @param SL Source Level measured from the field 
#' @param r Distance of call
#' @param PL The propogation loss (default is 20). Gathered from measurements.
#' @return The received level in dB relative to pressure reference
#' @export
recievedL2 <- function(SL, r, PL=20){
  SL2indiv = SL + 6
  
  SL2indiv_20m = SL2indiv - PL*log10(r)
  
  return(SL2indiv_20m)
}
