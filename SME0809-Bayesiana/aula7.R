library(invgamma)

norm_sigma <- function(samp, sigma = 40, mu = 900, tau0 = 20, alpha = 0.001, beta = 0.001){
  
  n <- length(samp)
  xbar <- mean(samp)
  
  ver <- function(x) 1/(sd(x))^(n/2) * (exp(1))^(-1/2) * sum(x - mu)^2 * 1/(sd(x))
  
  
  alpha.post <- alpha + n/2 #(tau0^(-2)*mu + n*sigma^(-2)*xbar)/ (tau0^(-2) + n * sigma^(-2))
  beta.post <- 1/2 * sum(samp - mu)^2 + beta#(tau0^(-2) + n*sigma^(-2))^(-1)
  
  theta <- seq(6, 12,0.05)
  
  
  tibble(theta = theta,
         priori = dgamma(theta,alpha, beta),
         post = dgamma(theta,alpha.post,beta.post),
         ver = ver(theta),
         pred = dnorm(theta,mu.post,tau0+sigma))
  
}

norm_sigma(rnorm(30, 0, 9)) %>% 
ggplot(aes(x = theta)) +
  geom_line(aes(y = priori, color = "priori")) +
  geom_line(aes(y = post, color = "posteriori")) +
  geom_line(aes(y = ver, color = "verossimilhança")) +
  scale_colour_brewer(name = "Distribuição", type = "qual", palette = "Dark2")+
  scale_x_continuous(name = expression(theta))+
  theme(axis.title.y=element_blank()) +
  ggtitle("Distribuição a priori da variancia")
