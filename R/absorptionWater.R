#' absorptionWater
#'
#' This function is a simplified method of calculating the absorption of sound
#' in water proposed in Ainslie and McColm, 1998 and based on Francois and Garrison, 1982.
#'
#' @param f The frequency of the sound source in kHz.
#' @param pH The average acidity or pH of the water. Default is 8.
#' @param t The average temperature of the water in Celsius. Default is 0.
#' @param s The average salinity of the water in parts per thousand (ppt). Default is 35 ppt.
#' @param z The depth in km.
#' @return The sound attenuation rate in dB/km
#' @export
#'
#' @examples
#' #How much are Malayan tapir calls (15 kHz) absorbed in a tropical region
#' #(30 deg C) assuming a humidity of 80% and standard pressure (101.325)?
#' absorption(15000, 101.325, 30, 80)

absorptionWater <- function(f, pH=8, t=0, s=35, z){

  #Boric acid relaxation frequency
  f1 <- 0.78*((s/35)^(1/2))*exp(T/26)

  #Magnesium sulphate relaxation frequency
  f2 <- 42*exp(T/17)

  #absorption coefficient
    #boric acid
  a <- 0.106*((f1*f^2)/(f^2+f1^2))*exp((pH-8)/0.56) +
    #magnesium sulphate
    0.52*(1+(T/43))*(s/35)*((f2*f^2)/(f^2+f2^2))*exp((-z)/6)+
    #viscousity absorption
    0.00049*f^2*exp(-((t/27)+(z/17)))
  return(a)
}
