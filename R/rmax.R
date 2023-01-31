#' wavelength and frequency
#'
#' This function finds the length of a sound wave, known as the wavelength (m), or
#' frequency of a sound (Hz), given the identity of the input. The speed of sound is the default in
#' air (340 m/s). You may calculate your own value for c relative to the conditions present
#' in your ecosystem of study via soundSpeed().
#'
#' @param b The known wavelength (m) or frequency (Hz).
#' @param c The speed of sound in m/s. As a general rule of thumb, however, c is equal to 1500 m/s in saltwater and 350 m/s in air. Note that the distinction between freshwater and saltwater is important. Sound speed is faster in saltwater than freshwater.
#' @return The unknown wavelength (m) or frequency (Hz) given the identity of input a.
#' @export
#'
#' @examples
#' # Given a frequency of 80 Hz in air, what is the wavelength?
#' wof(80000)
#' # Given a wavelength of 0.004 m in salt water, what is the frequency?
#' wof(0.004, c=1500)
rmax <- function(a=0,sl,nl,dt,d,geom="cyl",xaxis=25, n=300){

  ans <- sl-nl-dt-(10*log10(d/2))


  f1 <- function(x,ab=a){(ab*x + 10*log10(x*10))}
  f2 <- ans

  b <- optimize(function(t0) abs(f1(t0) - f2), interval = range(1:xaxis))

  return(b[[1]])
}
