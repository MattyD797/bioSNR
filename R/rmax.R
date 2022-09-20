rmax <- function(TL, boolR=T){
  r <- 10^(TL/10)/1000
  if(boolR){
    print(paste("The maximum range (rmax) is", r, "meters"))
  }
  return(r)
}
