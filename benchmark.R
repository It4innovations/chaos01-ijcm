# Load libraries ----------------------------------------------------------

library(microbenchmark)
library(ggplot2)
library(viridis)
library(Chaos01)

# Source R scripts --------------------------------------------------------

source("BB_chaos_function.R")
source("COG_chaos_function.R")
  
# Set parameters which were the same for all the runs in the paper --------

throw <- 2000 # how many iteration should be discarded at the beginning of the series
mu <- 3.5 # set parameter mu
length_c <- 100 # for how many parameters c should the computation be made
window_size <- 1000 # set window size for BB and COG
control_set_size <- window_size # set control set size for BB and COG
benchmark_rep <- 50  # set how many times should each computation run

res <- NULL

for(length_res in seq(10000, 100000, by = 10000)){  #set range and step for the input series size

  x <- gen.logistic(mu, length_res)
  x <- tail(x, -throw)

  a <- microbenchmark(COG = cog_chaos01(x, length_c, window_size, control_set_size, epsilon),
                      BB  = bb_chaos01(x, length_c, window_size, control_set_size),
                      MSD = testChaos01(x, c.gen = "equal"),
                      times = benchmark_rep)
  
  res <- rbind(res, data.frame(a, size = length_res))
}

# Reorder factor for visualization
res$expr <- factor(res$expr, c("MSD", "COG", "BB"))

ggplot(res, aes(x = size, 
                y =  time/10^9,
                color = expr)) +
  stat_smooth(method = "loess") +
  scale_x_continuous(name = "Number of Obserations") +
  scale_y_log10(name = "Time [seconds]") +
  scale_color_viridis("Method", discrete = T) +
  theme_bw()


