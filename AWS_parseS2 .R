AWS_parseS2 <- function(x,tile){

  ## Function to read Sentinel-2 metadata from AWS
  ##
  ## Usage:
  ##
  ## x ... Sentinel-2 filename
  ## tile ... S2 tile identifier
  ##
  ## returns: group column will be appended to input

  require(httr)

  # parse date
  if(grepl('OPER',x)){

    dt <- sub('.+V(\\d{8}).+','\\1',x)

  }else{

    dt <- str_extract(x,'\\d{8}')

  }

  utm <- str_extract(tile,'\\d{2}')
  MRG <- str_extract(tile,'[[:upper:]]{3}')

  url <- sprintf('http://sentinel-s2-l1c.s3.amazonaws.com/tiles/%s/%s/%s/%s/%i/%i/0/tileInfo.json',
                 utm,substr(MRG,1,1),substr(MRG,2,3),
                 substr(dt,1,4),as.integer(substr(dt,5,6)),as.integer(substr(dt,7,8)))

  response <- GET(url)

  cont <- content(response)

  cont$url <- url
  cont$code <- status_code(response)

  return(cont)

}
