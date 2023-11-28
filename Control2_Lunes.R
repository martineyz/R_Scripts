#1
sum(dpois(0:1,3)*(1-pnorm(-1,0:1,1)))
#Respuesta: 0.1878513


#2
library(mvtnorm)
muX=1;muY=2;sigmaX=sigmaY=2;p=-0.5
mu = c(muX,muY)
sigma = matrix(c(sigmaX**2,rep(sigmaX*sigmaY*p,2),sigmaY**2),2,2)
pmvnorm(c(0,0),c(2,4),mu,sigma = sigma)[1]
#Respuesta: 0.2854617


#3
1-(pgamma(30,4,0.2))**7
#Respuesta: 0.6825877


#4
1-pnorm(102, 45 * (1/3 + 2), sqrt(45) * sqrt(1/9))
#Respuesta: 0.9101438


#5
data = subset(Demanda_GAS, ESTACION == 'spring')
t.test(data$AVG, mu = 19, alternative = 'less')
#Respuesta: t = -1.3324, p-value = 0.09169, NO


#6
x = nrow(subset(Demanda_GAS, DEMANDA > 10000))
x
prop.test(x, nrow(Demanda_GAS), p = 0.5, alternative =  'greater', correct = F)
#Respuesta: X-squared = 15.244, p-value = 4.724e-05, SI

#7
library(fitdistrplus)
Y = Demanda_GAS$NUBOSIDAD/100
fitdist(Y, 'beta', 'mme')
#Respuesta: shape1 2.0234623, shape2 0.8153867


#8
est = fitdist(Y, 'beta', 'mme')$estimate
p1 = est[1]; p2 = est[2]
ks.test(Y, 'pbeta', p1, p2)
#Respuesta: D = 0.045971, p-value = 0.0003725, NO
