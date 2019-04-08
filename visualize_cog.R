# Load libraries ----------------------------------------------------------

library(Chaos01)

# Source R scripts --------------------------------------------------------

source("vis_functions.R")

# Set parameter, uncomment desired set ------------------------------------
#
# length_input - how many iteration should be used for the computation, generated series is of length length_input + throw
# mu - value of parameter mu
# c - value of parameter c
# xlim - set x-axis limits for visualization
# ylim - set y-acis limits for visualization
# bb.out - set number of bounding boxes which should be visualized (prevents cluttering)

# Figure 4 , Figure 6 
# length_input <- 10000
# mu <- 3.8
# c <- 1.15
# xlim <- NULL
# ylim <- NULL
# bb.out <- 30

# Figure 5 
# length_input <- 10000
# mu <- 3.5
# c <- 1.69
# xlim <- NULL
# ylim <- NULL
# bb.out <- 10000

# Figure 10 (a)
# length_input <- 10000
# mu <- 3.56994
# c <-  0.8948779
# xlim <- NULL
# ylim <- NULL
# bb.out <- 20

# Figure 10 (e)
# length_input <- 10000
# mu <- 3.56994
# c <-  0.8948779
# xlim <- c(-0.2905, -0.2845)
# ylim <- c(0.707, 0.7015)
# bb.out <- 20

# Figure 10 (b)
# length_input <- 10000
# mu <- 3.56995
# c <-  0.8948779
# xlim <- NULL
# ylim <- NULL
# bb.out <- 20

# Figure 10 (f)
length_input <- 10000
mu <- 3.56995
c <-  0.8948779
xlim <- c(-0.2905, -0.2845)
ylim <- c(0.707, 0.7015)
bb.out <- 20

# Figure 10 (c)
# length_input <- 2500
# mu <- 3.56994
# c <-  0.8948779
# xlim <- c(-0.2905, -0.2845)
# ylim <- c(0.707, 0.7015)
# bb.out <- 20
 
# Figure 10 (d)
# length_input <- 2500
# mu <- 3.56995
# c <-  0.8948779
# xlim <- c(-0.2905, -0.2845)
# ylim <- c(0.707, 0.7015)
# bb.out <- 20


# Set parameters which were the same for all the runs in the paper --------

throw <- 2000       # how many iteration should be discarded at the beginning of the series
window_size <- 2000 # BB and COG window size

# Generate input time series ----------------------------------------------

x <- gen.logistic(mu, length_input + throw)
x <- tail(x, -throw)

# Compute BB and COG ------------------------------------------------------

res <- bb_cog_chaos01_single_c(x, c, window_size)

# Visualize ---------------------------------------------------------------

# Run corresponding (based on data) visualization line to get the plot from the paper
# For exploration of the results You may use all the visualizations for any parameter settings

bb_cog_ggplot(res, type = "cog") # Figure 6
bb_cog_ggplot(res, type = "cog", window = FALSE) # Figure 6
bb_cog_ggplot(res, type = "cog", output = "pc_qc") # Figure 10 (a), (b)
bb_cog_ggplot(res, type = "cog", output = "test", xlim = xlim, ylim = ylim) # Figure 10 (c), (d), (e), (f)

bb_cog_ggplot(res, type = "bb", bb.out = bb.out) # Figure 4, 5
bb_cog_ggplot(res, type = "bb", window = FALSE, bb.out = bb.out) # Figure 4, 5

# ggsave(filename = "cog_chaos_long.jpeg", width = 184, height = 160, units = "mm")
 