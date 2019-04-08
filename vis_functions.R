# Load libraries ----------------------------------------------------------

library(Rcpp)
library(ggplot2)
library(viridis)
library(zoo)

# Define functions --------------------------------------------------------

# Simple rolling mean
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

# Compute BB and COG variables with defined parameter c and store
bb_cog_chaos01_single_c <- function(x, c, window_size){
  
  if(window_size >= length(x)){
    stop("'window_size' is greater or equal to the length of input time series. Choose smaller window size.")
  }
  
  length_res <- length(x)
  
  pc <- cumsum(x * cos(1:length_res*c))
  qc <- cumsum(x * sin(1:length_res*c))
  
  maxpc <- cummax(pc)
  minpc <- cummin(pc)
  maxqc <- cummax(qc)
  minqc <- cummin(qc)
  
  rollpccog <- c(rep(NA, (window_size - 1)), myrollmean(pc, window_size))
  rollqccog <- c(rep(NA, (window_size - 1)), myrollmean(qc, window_size))
  
  sizepc <- max(pc[1:window_size]) - min(pc[1:window_size])
  sizeqc <- max(qc[1:window_size]) - min(qc[1:window_size])
  
  l_res <- data.frame(
    pc = pc,
    qc = qc,
    # Non-windowed BB
    maxpc = maxpc,
    minpc = minpc,
    maxqc = maxqc,
    minqc = minqc,
    # Windowed BB
    rollmaxpc = rollmax(pc, window_size, na.pad = TRUE, align = "right"),
    rollminpc = -rollmax(-pc, window_size, na.pad = TRUE, align = "right"),
    rollmaxqc = rollmax(qc, window_size, na.pad = TRUE, align = "right"),
    rollminqc = -rollmax(-qc, window_size, na.pad = TRUE, align = "right"),
    # Non-windowed COG
    cogpc = cumsum(pc) / (1:length(pc)),
    cogqc = cumsum(qc) / (1:length(qc)),
    # Windowed COG
    rollpc = rollpccog,
    rollqc = rollqccog,
    iteration = 1:length_res)
  
  return(l_res)
}

bb_cog_ggplot <- function(df.res, type = "cog", output = "all", window = TRUE, xlim = NULL, ylim = NULL, bb.out = 20){
  
  if((type == "cog" & window)){
    df.viz <- df.res[,c("pc", "qc", "rollpc", "rollqc", "iteration")]
    colnames(df.viz) <- c("pc", "qc", "testpc", "testqc", "iteration")
  } else if((type == "cog" & window == FALSE)){
    df.viz <- df.res[,c("pc", "qc", "cogpc", "cogqc", "iteration")]
    colnames(df.viz) <- c("pc", "qc", "testpc", "testqc", "iteration")
  } else if((type == "bb" & window)){
    length_na <- sum(is.na(df.res$rollmaxpc))
    df.viz <- data.frame(pc = df.res$pc,
                         qc = df.res$qc,
                         testpc = c(rep(NA, length_na * 5), na.trim(df.res$rollminpc), na.trim(df.res$rollminpc), na.trim(df.res$rollmaxpc), na.trim(df.res$rollmaxpc), na.trim(df.res$rollminpc)),
                         testqc = c(rep(NA, length_na * 5), na.trim(df.res$rollminqc), na.trim(df.res$rollmaxqc), na.trim(df.res$rollmaxqc), na.trim(df.res$rollminqc), na.trim(df.res$rollminqc)),
                         iteration = c(rep(NA, length_na * 5), rep((length_na + 1):length(df.res$pc),5)))
    colnames(df.viz) <- c("pc", "qc", "testpc", "testqc", "iteration")
    ind <- rep(TRUE, length(df.res$pc) - length_na)
    ind[seq(1, length(df.res$pc) - length_na, length.out = bb.out)] <- FALSE
    ind <- c(rep(NA, length_na * 5), rep(ind, 5))
    df.viz$testpc[ind] <- NA
    df.viz$testqc[ind] <- NA
  } else if((type == "bb" & window == FALSE)){
    df.viz <- data.frame(pc = df.res$pc,
                         qc = df.res$qc,
                         testpc = c(df.res$minpc, df.res$minpc, df.res$maxpc, df.res$maxpc, df.res$minpc),
                         testqc = c(df.res$minqc, df.res$maxqc, df.res$maxqc, df.res$minqc, df.res$minqc),
                         iteration = df.res$iteration)
    colnames(df.viz) <- c("pc", "qc", "testpc", "testqc", "iteration")
    ind <- rep(TRUE, length(df.res$pc))
    ind[seq(1, length(df.res$pc), length.out = bb.out)] <- FALSE
    df.viz$testpc[ind] <- NA
    df.viz$testqc[ind] <- NA
  }
  
  if(type == "cog"){
    if(output == "all"){
      temp <- ggplot(df.viz,
                     aes(x = pc, y = qc)) +
        geom_point() +
        geom_point(aes(x = testpc, y = testqc, color = iteration)) +
        scale_color_viridis("Iteration") +
        xlab("") +
        ylab("") +
        theme_minimal()
    } else if(output == "pc_qc"){
      temp <- ggplot(df.viz, aes(x = pc, y = qc)) +
        geom_point()+
        xlab(bquote(p[c])) +
        ylab(bquote(q[c])) +
        theme_minimal()
    } else if(output == "test"){
      temp <- ggplot(df.viz,
                     aes(x = testpc, y = testqc, color = iteration)) +
        geom_point() +
        scale_color_viridis("Iteration") +
        xlab("") +
        ylab("") +
        theme_minimal()
    }
  } else if(type == "bb"){
    if(output == "all"){
      temp <- ggplot(df.viz,
                     aes(x = pc, y = qc)) +
        geom_point(alpha = 0.1) +
        geom_path(aes(x = testpc, y = testqc, group = iteration, color = iteration)) +
        scale_color_viridis("Iteration") +
        xlab("") +
        ylab("") +
        theme_minimal()
    } else if(output == "pc_qc"){
      temp <- ggplot(df.viz, aes(x = pc, y = qc)) +
        geom_point()+
        xlab(bquote(p[c])) +
        ylab(bquote(q[c])) +
        theme_minimal()
    } else if(output == "test"){
      temp <- ggplot(df.viz,
                     aes(x = testpc, y = testqc, group = iteration, color = iteration)) +
        geom_path() +
        scale_color_viridis("Iteration") +
        xlab("") +
        ylab("") +
        theme_minimal()
    }
  }
  
  if(!all(is.null(xlim), is.null(ylim))){
    temp <- temp +
      coord_cartesian(xlim = xlim, ylim = ylim)
  }
  
  temp
}
