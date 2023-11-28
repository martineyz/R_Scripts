#Probabilidades conjuntas
#1
XY = function(x,y){dpois(x,7)*dbinom(y,x,0.3)}

#a)
XY(7,4)

#b)
pbinom(4,9,0.3)

#c)
XY(10,7)/dpois(7,7*0.3)

#d)
sum(XY(0:10,7)/dpois(7,7*0.3))

#2
XY = function(x,y){dbinom(x,12,0.7)*dbinom(y,x,0.3)}

#a)
1 - pbinom(4,12,0.21)

#b)
sum(dbinom(3:7,6,0.3))

#c)
XY(6,3)/dbinom(3,12,0.21)

#3
library(mvtnorm)
muX = muY = 2; sdX = 1; sdY = 2; rho = 0.3
mu = c(muX, muY)
sigma = matrix(c(sdX**2, rep(sdX*sdY*rho,2), sdY**2),2,2)

#a)
pmvnorm(c(1,1),c(2,3),mu,sigma=sigma)[1]

#b)
pnorm(2.3,muX,sdX)

#c)
pmvnorm(c(1,-Inf),c(Inf,2.3),mu,sigma=sigma)[1]

#Probabilidades TLC y valores extremos
tlc = function(x, .mean, .var, n, type=0, correction=F){
  mu = ifelse(type == 0, n * .mean, .mean)
  sigma = ifelse(type == 0, sqrt(n) * sqrt(.var), sqrt(.var) / sqrt(n))
  x = ifelse(correction, ifelse(type == 0, x + 0.5, x + 0.5/n), x)
  return(pnorm(x, mu, sigma))
}

Fmin = function(p, n){1-(1-p)**n}
Fmax = function(p, n){p**n}

#1
pnorm(4.2+0.5/60,4,sqrt(4)/sqrt(60))
tlc(4.2,4,4,60,1,T)

#2
tlc(4.2, 4, 1, 45, 1) - tlc(3, 4, 1, 45, 1)

#3
1 - tlc(1.1, 1, 1/2, 40, 1)

#4
1 - tlc(0.25-1/120, 0.2, 0.2*(1 - 0.2), 120, 1, T)

#5
1-Fmax(punif(5.5,3,6), 5)

#6
Fmax(pexp(2,0.5),10)

#7
1-Fmin(pnorm(-0.3,0,2),4)

#8
Fmin(pbeta(0.15,3,4),14)


#Estimación de parámetros
library(readxl)
library(fitdistrplus)

#ENS.xlsx
ENS <- read_excel("RData/ENS.xlsx")

#1
fitdist(ENS$COLES, distr = 'lnorm', method = 'mme')

#2
fitdist(ENS$PAD, distr = 'logis', method = 'mle')

#3
fitdist(ENS$COLES, distr = 'gamma', method = 'mle')

#4
fitdist(ENS$PAS, distr = 'gamma', method = 'mme')

#5
fitdist(ENS$HDL, distr = 'weibull', method = 'mle')

#Abalon.xlsx
Abalon <- read_excel("RData/Abalon.xlsx")

#1
fitdist(Abalon$largo, distr = 'weibull', method = 'mle', fix.arg = list(scale = 16))

#2
fitdist(Abalon$diametro, distr = 'gamma', method = 'mle', fix.arg = list(rate = 2))

#3
fitdist(Abalon$anillos, distr = 'pois', method = 'mme')

#Test de hipótesis
library(readxl)
library(fitdistrplus)
library(TeachingDemos)

#ENS.xlsx
ENS <- read_excel("RData/ENS.xlsx")

#1
t.test(ENS$COLES,mu=195,alternative = "greater")

#2
t.test(ENS$PAD,mu=75,alternative = "two.sided")

#3
test = prop.test(length(ENS$FUMADOR[ENS$FUMADOR==0]), n=nrow(ENS), p=0.5, alternative = 'greater', correct = F)
test$p.value
test

#4
prop.test(length(ENS$DIABETES[ENS$DIABETES==1]), n=nrow(ENS), p=0.1, alternative = 'greater', correct = F)

#5
est = fitdist(ENS$COLES, distr = 'lnorm', method = 'mle')$estimate
lambda = est[1]; zeta = est[2]
ks.test(ENS$COLES, 'plnorm', lambda, zeta)

#6
est = fitdist(ENS$LDL, distr = 'gamma', method = 'mle')$estimate
k = est[1]; nu = est[2]
ks.test(ENS$LDL, 'pgamma', k, nu)

#Abalon.xlsx
Abalon <- read_excel("RData/Abalon.xlsx")

#1
t.test(Abalon$largo,mu=14,alternative = "less")

#2
est = fitdist(Abalon$pesot, distr = 'norm', method = 'mle')$estimate
mu = est[1]; sigma = est[2]
ks.test(Abalon$pesot, 'pnorm', mu, sigma)

#3
est = fitdist(Abalon$largo, distr = 'weibull', method = 'mle')$estimate
p1 = est[1]; p2 = est[2]
ks.test(Abalon$largo, 'pweibull', p1, p2)
