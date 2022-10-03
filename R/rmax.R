#' This function solves the sonar equation for the maximum range, for cylindrical spreading and no absorption
#' e.g seek the range at wich the TL =  SL - NL - DT;

rmax <- function(TL, boolR=T){
  r <- 10^(TL/10)/1000
  if(boolR){
    print(paste("The maximum range (rmax) is", r, "meters"))
  }
  return(r)
}
