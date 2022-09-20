TLcylindrical <- function(SL, NL, DT, TR, boolR=T){
  #TLc = SL - NL - DT - 10log10(TransitionRange)
  TLc <- SL - NL - DT - (10*log10(TR))
  if(boolR){
    print(paste("The transition loss (TL) associated with cylindrical spreading is:", TLc, "dB ref 1 \u00B5P"))
  }
  return(TLc)
}
