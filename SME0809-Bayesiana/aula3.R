
library(tidyverse)

bern <- function(n = 27, p = 11/27, a = 1/2, b = 1/2){

   y <- rep(1, n*p)
   alpha <- a
   beta <- b
   ver <- function(x) x^(n*p)*(1-x)^(n-n*p)
   
   
   a.post <- n*p + alpha
   b.post <- n - n*p + beta
   
   theta <- seq(0,1,0.01)
   
   
   tibble(theta = theta, priori = dbeta(theta,alpha,beta), post = dbeta(theta,a.post,b.post), ver = ver(theta))
}
