#' Average dBs
#'
#' This function averages dB levels
#'
#' @param dbs A vector of dB values
#' @param PL The reference value where pressure dB is 20 and intensity dB is 10
#' @return The average of a series of dB levels
#' @export
avgDB <- function(dbs, PL = 20){
  RL2 <- PL*log10(mean(10^(dbs/PL)))
  return(RL2)
}

