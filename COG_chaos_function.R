library(Rcpp)

cppFunction('
  NumericVector myrollmean( NumericVector x, double width) {
    double temp=0;
    unsigned int n=x.size(), i, counter=0;
    NumericVector out(n-width+1);
    
    for(i = 0; i < width; i++) {
      temp += x[i];
    }
    out[0] = temp / width;
    counter += 1;
    for( i=width; i<n; i++ ) {
      temp = temp - x[i-width] + x[i];
      out[counter] = temp / width;
      counter += 1;
    }
    return out;
  }')

cog_chaos01 <- function(x, length_c, window_size, epsilon, out = "median"){

  if(window_size >= length(x)){
    stop("'window_size' is greater or equal to the length of input time series. Choose smaller window size.")
  }
  
  c_all <- seq(pi/5, 4*pi/5, length.out = length_c)
  Kc <- rep(NA, length_c)
  length_res <- length(x)
  
  for(i in 1:length_c)
  {
    c <- c_all[i]
    pc <- cumsum(x * cos(1:length_res*c))
    qc <- cumsum(x * sin(1:length_res*c))
    # maxpc <- cummax(pc)
    # minpc <- cummin(pc)
    # maxqc <- cummax(qc)
    # minqc <- cummin(qc)
    rollpccog <- c(rep(NA, (window_size - 1)), myrollmean(pc, window_size))
    rollqccog <- c(rep(NA, (window_size - 1)), myrollmean(qc, window_size))
    
    sizepc <- max(pc[1:window_size]) - min(pc[1:window_size])
    sizeqc <- max(qc[1:window_size]) - min(qc[1:window_size])
    
    res <- sum((rollpccog[(2 * window_size):length_res] - rollpccog[window_size:(length_res - window_size)])^2 + 
                 (rollqccog[(2 * window_size):length_res] - rollqccog[window_size:(length_res - window_size)])^2
               > (sizepc / (window_size/epsilon))^2 + (sizeqc / (window_size/epsilon))^2)
    Kc[i] <- res/(length(x) - (2 * window_size - 1))
    
  }
  if(out == "median"){
    return(median(Kc))
  } else if(out == "all"){
    return(Kc)
  }
}
