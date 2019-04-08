gen.henon <- function(a, b, iter = 5000, x0 = (sqrt(609)-7)/28, y0 = 3*(sqrt(609)-7)/280){
  x <- rep(NA, iter)
  y <- rep(NA, iter)
  
  x[1] <- x0
  y[1] <- y0
   
  for(i in 2:iter){
    xtemp <- 1 - a * x0^2 + y0
    ytemp <- b * x0
    x[i] <- xtemp
    y[i] <- ytemp
    x0 <- xtemp
    y0 <- ytemp
  }
  
  return(list(x = x, y = y))
}
