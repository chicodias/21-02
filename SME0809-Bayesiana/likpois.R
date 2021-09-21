y1 <- c(5, 0, 3, 2, 1, 2, 1, 1, 2, 1)

y2 <- y1+3; y3 <- y1+7;

lik.pois <- function(lambda, dados) {
  loglik <- function(theta, y) {
    logl <- sum(y)*log(theta) - length(y)*theta
    exp(logl)
  }
  sapply(lambda, loglik, y = dados)
}

ver.poi <- function(y) exp(-n*theta)*(theta*n)^sum(y)/prod(fatorial(y))

theta <- seq(0, 30, 0.1)


loglik <- sapply(theta, lik.pois, dados = rpois(10,1))

par(mar = c(4,4,4,4))
plot(theta, loglik, type = 'l',
     xlab = expression(theta),
     ylab = expression(L(theta)))
par(new=T)

loglik <- sapply(theta, lik.pois, dados = y2)
plot(theta, loglik,
     type ="l", axes = F,
     frame = T, ann = F, col =2)
axis(4, col.axis = 2, col =2)
mtext(expression(L(theta)),side =4, line =3, col =2)
par(new=T)

loglik <- sapply(theta, lik.pois, dados = y3)
plot(theta, loglik,
     type ="l", axes = F,
     frame = T, ann = F, col = 3)
axis(3, col.axis = 3, col = 3)
mtext(expression(L(theta)),side =3, line =3, col =3)

