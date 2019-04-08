library(zoo)

bb_chaos01 <- function(x, length_c, window_size, control_set_size, threshold = 0.995, out = "median"){

  
  c_all <- seq(pi/5, 4*pi/5, length.out = length_c)
  Kc <- rep(NA, length_c)
  
  for(i in 1:length_c)
  {
    c <- c_all[i]
    pc <- cumsum(x * cos(1:length(x)*c))
    qc <- cumsum(x * sin(1:length(x)*c))
    maxpc <- cummax(pc)
    minpc <- cummin(pc)
    maxqc <- cummax(qc)
    minqc <- cummin(qc)
    
    sizepc <- max(pc[(control_set_size - window_size):control_set_size]) - min(pc[(control_set_size - window_size):control_set_size])
    sizeqc <- max(qc[(control_set_size - window_size):control_set_size]) - min(qc[(control_set_size - window_size):control_set_size])
    
    sizepc2 <- maxpc[length(x)] - minpc[length(x)] 
    sizeqc2 <- maxqc[length(x)] - minqc[length(x)]
    
    Kc[i] <- ((sizepc * sizeqc) / (sizepc2 * sizeqc2)) < threshold
  }
  if(out == "median"){
    return(median(Kc))
  } else if(out == "all"){
    return(Kc)
  }
}


