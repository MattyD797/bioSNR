#' Subtract dBs
#'
#' This function subtracts dB levels
#'
#' @param dbs A vector of dB values
#' @param PL The reference value where pressure dB is 20 and intensity dB is 10
#' @return The subtraction of a series of dB levels
#' @export
subDB <- function(dbs, PL = 20){
  RL2 <- PL*log10(sub(10^(dbs/PL)))
  return(RL2)
}

#' Recursive Subtraction
#'
#' This function subtracts dB levels
#'
#' @param x A series of values to subtract
#' @return The solution of subtracting all values
sub <- function(x){
  if(length(x) == 1){
    return(x)
  } else {
    y <- x[1] - x[2]
      return(sub(y))
    }
}
