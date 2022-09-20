#' Add dBs
#'
#' This function adds dB levels
#'
#' @param dbs A vector of dB values
#' @param PL The reference value where pressure dB is 20 and intensity dB is 10
#' @return The addition of a series of dB levels
#' @export
addDB <- function(dbs, PL = 20){
  RL2 <- PL*log10(sum(10^(dbs/PL)))
  return(RL2)
}
