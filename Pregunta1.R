#poblacion X = N(mu, sigma^2)
mu = 93.5
sigma = 5.7

curve(dnorm(x,mu,sigma),xlim = c(80,120))

#a)
set.seed(123)

Y = function(i){sum(rnorm(4, mu, sigma))}
Y(2)

Y10000 = sapply(1:10000,Y)
Y10000

hist(Y10000)
mean(Y10000)

4*mu

#b)
X = function(i){sum(rnorm(100, mu, sigma))}
X10000 = sapply(1:10000,X)
var(X10000)
hist(X10000)

100*sigma^2

curve(dnorm(x,mu,sigma),xlim = c(80,120))
#c)
1-pnorm(103, mu, sigma)
#d)
xbar= function(i){mean(rnorm(4, mu, sigma))}
xbar500000 = sapply(1:500000,xbar)
xbar500000
hist(xbar500000)
mean(xbar500000>98)

1-pnorm(98, mu, sigma/sqrt(4))
#e)
Ssq= function(i){var(rnorm(100, mu, sigma))}
Ssq500000 = sapply(1:500000,Ssq)
hist(Ssq500000)
mean(Ssq500000>32)

hist(Ssq500000*(100-1)/sigma^2,frequency = FALSE)
32*(100-1)/sigma^2
curve(dchisq(x, 100-1),col ="red",add=TRUE)
mean(Ssq500000>32)
