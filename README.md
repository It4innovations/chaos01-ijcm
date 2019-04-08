# chaos01_ijcm
This repository contains scripts for computation of the two new approaches of 0-1 test for chaos.

There are 4 main scripts used to provide visualizations which illustrate how the new approaches work and to compare their performance to the original method.

These scripts are benchmark.R, compute_logistic_results.R, compute_henon_results.R, visualize_cog.R.
It is possible to get the results by uncommenting the lines with desired settings and then sourcing the scripts.
For interactive (and much more enjoyable work) it is advised to open the scripts in IDE such as [RStudio](https://www.rstudio.com/).

Repository contains additional 4 scripts which contains functions for computation of the "Bounding box" version of 0-1 test for chaos (BB), "Centre of gravity" version of 0-1 test for chaos (COG), iterations of Henon map and for visualization of results.

If You are interested only in computing the BB, or COG, it is possible to source the BB_chaos_function.R and COG_chaos_function.R respectively. Functions "bb_chaos01" and "cog_chaos01" will be loaded into global environment.

To run these scripts You need to install several other R packages:
[microbenchmark](https://cran.r-project.org/web/packages/microbenchmark/index.html), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/), [viridis](https://cran.r-project.org/web/packages/viridis/), [Chaos01](https://cran.r-project.org/web/packages/Chaos01/), and [zoo](https://cran.r-project.org/web/packages/zoo/)

You can do this by running following command:
```r
 install.packages(c("microbenchmark", "ggplot2", "viridis", "Chaos01", "zoo"))
 ```
