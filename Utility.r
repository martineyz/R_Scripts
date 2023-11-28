install.packages("readxl")
install.packages("mvtnorm")
install.packages("fitdistrplus")
install.packages("TeachingDemos")

library(readxl)
library(mvtnorm)
library(fitdistrplus)
library(TeachingDemos)

tlc = function(x, .mean, .var, n, type=0, correction=F){
  mu = ifelse(type == 0, n * .mean, .mean)
  sigma = ifelse(type == 0, sqrt(n) * sqrt(.var), sqrt(.var) / sqrt(n))
  x = ifelse(correction, ifelse(type == 0, x + 0.5, x + 0.5/n), x)
  return(pnorm(x, mu, sigma))
}

Fmin = function(p, n){1-(1-p)**n}
Fmax = function(p, n){p**n}