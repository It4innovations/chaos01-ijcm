# Load libraries ----------------------------------------------------------

library(Chaos01)
library(ggplot2)
library(viridis)

# Source R scripts --------------------------------------------------------

source("BB_chaos_function.R")
source("COG_chaos_function.R")

# Set parameter, uncomment desired set ------------------------------------

# Figure 7
# mu_all <- seq(3.5, 4, by = 0.001) # set range and step for parameter mu

# Figure 8
mu_all <- seq(3.567, 3.573, by = 0.00005) # set range and step for parameter mu

# Set parameters which were the same for all the runs in the paper --------

length_res <- 8000 # how many iteration should be used for the computation, generated series is of length length_input + throw
throw <- 2000      # how many iteration should be discarded at the beginning of the series

length_c <- 100 # for how many parameters c should the computation be made
window_size <- 2000 # set window size for BB and COG
control_set_size <- length_res / 2 # set control set size for BB and COG
epsilon <- 4.5 # set threshold for COG
threshold <- 0.995 # set threshold for BB

c_all <- seq(pi/5, 4*pi/5, length.out = length_c)
length_mu <- length(mu_all)
res_mu_cog <- rep(NA, length_mu)
res_mu_bb <- rep(NA, length_mu)
res_mu_msd <- rep(NA, length_mu)

# Computation -------------------------------------------------------------

for(j in 1:length_mu)
{
  
  mu <- mu_all[j]
  x <- gen.logistic(mu, length_res + throw)
  x <- tail(x, -throw)
  
  res_mu_cog[j] <- cog_chaos01(x, length_c, window_size, epsilon)
  res_mu_bb[j] <- bb_chaos01(x, length_c, window_size, control_set_size, threshold)
  res_mu_msd[j] <-  testChaos01(x, c.gen = "equal")
}

results <- data.frame(
  mu = rep(mu_all,3),
  res = c(res_mu_bb, res_mu_cog, res_mu_msd),
  method = c(rep("BB", length_mu),
             rep("COG", length_mu),
             rep("MSD", length_mu))
)
results$method <- factor(results$method, c("MSD", "COG", "BB"))

# Visualization -----------------------------------------------------------

ggplot(results, aes(x = mu, y = res, color = method)) +
  geom_point() +
  facet_grid(method ~ .) +
  scale_color_viridis(name = "Method", discrete = T) +
  xlab(expression(mu)) +
  ylab("0-1 test for chaos") +
  theme_minimal() +
  theme(legend.position = "none")

# ggsave(filename = "results_log_focus.eps", width = 184, height = 104, units = "mm")