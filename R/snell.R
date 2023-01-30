#' Snell's Law
#'
#' This function is a reformated version of Snell's law that finds reflection or refraction angle given the two mediums longitudinal
#' wave velocities.
#'
#' @param ang The known angle of relfection or refraction
#' @param v1 The longitudinal wave velocity of medium where angle (refraction or reflection) is known given in m/s.
#' @param v2 The longitudinal wave velocity of medium where angle (refraction or reflection) is unknown given in m/s.
#' @return The opposing angle of reflection or refraction, respectfully
#' @export
#'
#' @examples
#'
snell <- function(ang, v1, v2){
  #Snell's law
  ang2 <- (ang/v1)*v2
  return(ang2)
}
