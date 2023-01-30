#' wavelength and frequency
#'
#' This function finds the length of a sound wave, known as the wavelength (m), or
#' frequency of a sound (Hz), given the identity of the input. The speed of sound is the default in
#' air (340 m/s). The speed of sound, c, can be calculated via soundSpeed().
#'
#' @param b The known wavelength (m) or frequency (Hz).
#' @param c The speed of sound in m/s. Sound travels ~340 m/s in air and ~1500 m/s in salt water.
#' @return The unknown wavelength (m) or frequency (Hz) given the identity of input a.
#' @export
#'
#' @examples
#' # Given a frequency of 80 Hz in air, what is the wavelength?
#' wof(80000)
#' # Given a wavelength of 0.004 m in salt water, what is the frequency?
#' wof(0.004, c=1500)
wof <- function(b, c = 340){
  a <- c/b
  return(a)
}

