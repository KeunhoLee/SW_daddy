convert_sec <- function(x){
  if(length(x) == 3){
    secs <- as.numeric(x[1])*3600 + as.numeric(x[2])*60 + as.numeric(x[3])
  } else {
    secs <- as.numeric(x[1])*60 + as.numeric(x[2])
  }
  
}
