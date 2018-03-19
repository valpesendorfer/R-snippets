to_decimal <- function(D,d,m,s){

  ## Function to convert DMS to DD cordinates in WGS84
  ##
  ## Usage:
  ##
  ## D ... cardinal direction
  ## d ... degree
  ## m ... minuite
  ## s ... second

  dd <- d+(m/60)+(s/3600)

  if(D == 'S' | D== 'W'){dec <- dec * -1}

  return(dd)

}
