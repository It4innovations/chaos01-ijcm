# Load libraries ----------------------------------------------------------

library(Chaos01)
library(ggplot2)
library(viridis)

# Source R scripts --------------------------------------------------------

source("BB_chaos_function.R")
source("COG_chaos_function.R")
source("henon.R")

# Set parameter, uncomment desired set ------------------------------------

# Figure 9
a_all <- seq(1, 1.42, by = 0.0005) # set the range and step for parameter a
b <- 0.3 # set parameter b
  
# Set parameters which were the same for all the runs in the paper --------

length_res <- 10000 # how many iteration should be used for the computation, generated series is of length length_input + throw
throw <- 3000       # how many iteration should be discarded at the beginning of the series

length_c <- 100 # for how many parameters c should the computation be made
window_size <- 4000 # set window size for BB and COG
control_set_size <- length_res / 2 # set control set size for BB and COG
epsilon <- 4.5 # set threshold for COG
threshold <- 0.995 # set threshold for BB

c_all <- seq(pi/5, 4*pi/5, length.out = length_c)
length_a <- length(a_all)
res_mu_cog_x <- rep(NA, length_a)
res_mu_bb_x <- rep(NA, length_a)
res_mu_msd_x <- rep(NA, length_a)

res_mu_cog_y <- rep(NA, length_a)
res_mu_bb_y <- rep(NA, length_a)
res_mu_msd_y <- rep(NA, length_a)

# Computation -------------------------------------------------------------

for(j in 1:length_a)
{
  a <- a_all[j]
  x <- gen.henon(a, b, length_res + throw)
  y <- tail(x[[2]], -throw)
  x <- tail(x[[1]], -throw)
  
  res_mu_cog_x[j] <- cog_chaos01(x, length_c, window_size, epsilon)
  res_mu_bb_x[j] <- bb_chaos01(x, length_c, window_size, control_set_size, threshold)
  res_mu_msd_x[j] <-  testChaos01(x, c.gen = "equal")
  
  res_mu_cog_y[j] <- cog_chaos01(x, length_c, window_size, epsilon)
  res_mu_bb_y[j] <- bb_chaos01(x, length_c, window_size, control_set_size, threshold)
  res_mu_msd_y[j] <-  testChaos01(x, c.gen = "equal")
}

results <- data.frame(
  a = rep(a_all,6),
  res = c(res_mu_bb_x, res_mu_cog_x, res_mu_msd_x, res_mu_bb_y, res_mu_cog_y, res_mu_msd_y),
  method = c(rep("BB", length_a),
             rep("COG", length_a),
             rep("MSD", length_a)),
  variable = c(rep("x", length_a * 3), rep("y", length_a * 3))
)
results$method <- factor(results$method, c("MSD", "COG", "BB"))

# Visualization -----------------------------------------------------------

ggplot(results, aes(x = a, y = res, color = method)) +
  geom_point() +
  facet_grid(variable + method ~ .) +
  scale_color_viridis(name = "Method", discrete = T) +
  xlab("a") +
  ylab("0-1 test for chaos") +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave(filename = "results_henon.eps", width = 184, height = 104, units = "mm")
