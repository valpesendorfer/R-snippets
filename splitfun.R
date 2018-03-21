splitfun <- function(x,n){

## Function randomly splitting data into groups
##
## Usage:
##
## x ... dataframe or matrix
## n ... number of groups
##
## returns: group column will be appended to input


  # empty group vector
  grp <- rep(0,nrow(x))

  # handle uneven number of observations per group, shuffle final distribution to avoid last group bias
  obs <- rep(floor(nrow(x)/n),(n-1))

  obs <- sample(c(obs,nrow(x)-sum(obs)))

  # row vector for observation selection
  rws <- 1:nrow(x)

  # first group
  ix <- sample(rws,obs[1],replace = F)

  grp[ix] <- 1

  # exclusion vector
  exc <- ix

  for (ii in 2:(n-1)){


    ix <- sample(rws[-exc],obs[ii],replace = F)

    grp[ix] <- ii

    exc <- c(exc,ix)

  }

  # last group
  ix <- rws[-exc]

  grp[ix] <- n

  # append to input
  x <- cbind(x,grp)

  return(x)
}
