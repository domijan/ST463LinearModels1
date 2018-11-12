## ----fig.width=4.5,fig.height=4.5,results='hide',echo=FALSE, fig.align='center'----
plot(Temp, Fuel)
mylm <- lm(Fuel ~ Temp)
abline(mylm)
segments(Temp, Fuel, Temp, predict(mylm), col="red")

## ------------------------------------------------------------------------

x <- Temp
y <- Fuel
n <- 8

cbind(x, y, xsq = x^2, ysq = y^2, xy = x * y)
sum(x)
sum(y)
mean(x)
mean(y)
sum(x^2)
sum(y^2)
sum(x * y)
Sxx <- sum(x^2) - n * mean(x)^2
Sxx

Syy <- sum(y^2) - n * mean(y)^2
Syy

Sxy <- sum(x * y) - n * mean(x) * mean(y)
Sxy

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----


plot(Temp, Fuel)
mylm <- lm(Fuel ~ Temp)
abline(mylm)
#segments(Temp, Fuel, Temp, predict(mylm), col="red")
x2 <- c(40,  50)
new <- data.frame(Temp = c(40,  50))
segments(x2, 0, x2, predict(mylm, new), col="red", lty=2)
segments(0, predict(mylm, new), x2, predict(mylm, new), col="red", lty=2)

## ----fig.width=4,fig.height=4 , results='hide',echo=FALSE, fig.align='center'----

#  NORMAL CURVES AROUND THE PREDICTED REGRESSION LINE VALUES.

plot(Temp, Fuel, xlim = c(15, 70), ylim = c(6, 15))
mylm <- lm(Fuel ~ Temp)
abline(mylm)

#plot.norm(2,mylm, 5, mycol = 'pink', howmany=150)
plot.norm(3,mylm, 5, mycol = 'pink', howmany=150)
plot.norm(4,mylm, 5, mycol = 'pink', howmany=150)
plot.norm(5,mylm, 5, mycol = 'pink', howmany=150)
#plot.norm(6,mylm, 5, mycol = 'pink', howmany=150)
points(Temp, Fuel)

## ----echo = TRUE, fig.align='center'-------------------------------------


fit <- lm(Fuel~Temp)
summary(fit)
anova(fit)

## ----echo = TRUE, fig.align='center'-------------------------------------
#~~~~~~~~~~~~~~~ Simulation

#~~~~~~~~~~~~~~~ True model parameters:
beta_0 <- 1
beta_1 <- 2
sigma_sq <- 2 

# Simulate a dataset:
n <- 100 #number of observations in the sample

x <- runif(n, -5, 5)
e <- rnorm(n, 0, sqrt(sigma_sq)) 
y <- beta_0 + x * beta_1  + e

plot(x, y)
abline(beta_0, beta_1, col = 2 ) # true model
points(mean(x), mean(y), col = 3)
fit <- lm(y ~ x) #fit the model
abline(coef(fit)[1], coef(fit)[2], col = 3)

## ------------------------------------------------------------------------
# estimated coefficients
coef(fit)
s <- sqrt(sum(residuals(fit)^2)/(n-2))
s^2


## ------------------------------------------------------------------------
# some useful calculations for dist of estimates

x_bar <- mean(x)
Sxx <- sum(x^2) - n * (x_bar^2)

var_beta_0 <- sigma_sq * (1/n + x_bar^2/Sxx)
var_beta_1 <- sigma_sq /Sxx

est_cov <- -sigma_sq *  x_bar/Sxx # estimated covariance from one sample

se_fit <- sqrt(sigma_sq * (1/n + (x - x_bar)^2/Sxx))

## ----echo = TRUE, fig.align='center'-------------------------------------

#~~~~~~~~~~~~~~~~~~~~ Repeat the simulation

N <- 500 #number of simulations
estimates <- matrix(0, N, 3) # to save param estimates



for (i in 1:N)
{
  x <- runif(n, -5, 5)
  e <- rnorm(n, 0, sqrt(sigma_sq)) 
  y <- beta_0 + x * beta_1  + e
  
  fit <- lm(y ~ x)
  estimates[i, 1] <- coef(fit)[1]
  estimates[i, 2] <- coef(fit)[2]
  estimates[i, 3] <- anova(fit)[["Mean Sq"]][2] # sigamsq
}

plot(x,y)
for (i in 1:50)abline(estimates[i, 1], estimates[i, 2], col = "grey") # N is too many points


## ----echo = TRUE, fig.align='center'-------------------------------------


#~~~~~~~~~~~~~~ plot the estimates

#beta_0
hist(estimates[,1], breaks = 50, freq = FALSE, xlab = expression(hat(beta)[0]), main =  expression(hat(beta)[0]))
curve(dnorm(x, beta_0, sqrt(var_beta_0) ), col = 2, add = TRUE)


#beta_1
hist(estimates[,2], breaks = 50, freq = FALSE, xlab = expression(hat(beta)[1]), main =  expression(hat(beta)[1]))
curve( dnorm(x, beta_1, sqrt(var_beta_1)) , col = 2, add = TRUE)

#sigma_sq
hist((n-2)/sigma_sq *estimates[,3], breaks = 50, freq = FALSE, xlab = expression(hat(sigma)^2), main =  expression(hat(sigma)^2))
curve( dchisq(x, df = n-2), col = 2, add = TRUE)



## ----fig.width=4,fig.height=4 , results='hide',echo=FALSE, fig.align='center'----

plot(Temp, Fuel)
mylm <- lm(Fuel ~ Temp)
abline(mylm)
newx <- seq(30,60)
CI <- predict(mylm, newdata = data.frame(Temp = newx), interval = c("confidence"),
level = 0.95,type="response")
lines(newx, CI[,2],col="red",lty=2)
lines(newx, CI[,3],col="red",lty=2)

PI <- predict(mylm, newdata = data.frame(Temp = newx), interval = c("prediction"),
 level = 0.95,type="response")
lines(newx, PI[,2],col="red",lty=2)
lines(newx, PI[,3],col="red",lty=2)

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
#prediction and confidence intervals
plot(heights$Mheight,heights$Dheight,xlim = c(50, 75), ylim = c(50, 75), xlab = "mother height (inches)", ylab = "daughter height (inches)")
attach(heights)
mylm2<- lm(Dheight~Mheight, data = heights)
abline(mylm2,lty = 1)


newx <- seq(50,90)
CI <- predict(mylm2, newdata = data.frame(Mheight = newx), interval = c("confidence"),
level = 0.95,type="response")
lines(newx, CI[,2],col="red",lty=2)
lines(newx, CI[,3],col="red",lty=2)

PI <- predict(mylm2, newdata = data.frame(Mheight = newx), interval = c("prediction"),
 level = 0.95,type="response")
lines(newx, PI[,2],col="red",lty=2)
lines(newx, PI[,3],col="red",lty=2)


## ----echo = FALSE--------------------------------------------------------
SOURCE <- c("Regression", "Error", "Total")
df <- c("1", "n-2", "n-1")
SS <- c("SSR", "SSE", "SST")
MS <- c("MSR = SSR/1", "MSE = SSE/(n-2)", "")
Fstat <- c("MSR/MSE", "", "")

kable(data.frame(SOURCE=SOURCE, df = df, SS = SS, MS = MS, F = Fstat))

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----
x <-runif(15)
 y <- x
 plot(x,y, pch = 16)
abline(0,1)

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----
x <-runif(50) * 10
 y <- runif(50) * 0.5 + 0.25
 plot(x, y, ylim = c(0,1),xlim = c(0,11), pch = 16)
abline(h = mean(y), lty = 2)
 text(11, 0.375, expression(bar(y)),cex = 1.2)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----

#father.son<-read.table("http://stat-www.berkeley.edu/users/juliab/141C/pearson.dat",sep=" ")[,-1]

 father.son <- read.table("data/Pearson.txt", header = TRUE)
names(father.son) <- c("father", "son")

father.son2 <- as.data.frame(sweep(data.matrix(father.son), 2, apply(father.son, 2, mean)))
bivn.kde <- kde2d(father.son2$father,father.son2$son, h = 8, n = 70)

plot(father.son2, col = "grey40", xlim = c(-10, 10) , ylim = c(-10, 10))
contour(bivn.kde, add=TRUE, drawlabels = FALSE)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
par(mfrow=c(1,1), oma =  c(0, 0, 0, 0))
persp(bivn.kde, phi = 45, theta = 30, xlab = "X")

## ----fig.width=5,fig.height=9, results='hide',echo=FALSE, fig.align='center'----
par(mfrow=c(3,2))
muA <- c(1,1)

traineg <- rbind(mvrnorm(150, muA, matrix(rep(1,4), 2, 2)))
plot(traineg, main = paste("r = ",  cor(traineg[,1], traineg[, 2])), xlab = "x", ylab = "y" )  
traineg <- rbind(mvrnorm(150, muA, matrix(c(1, 0.9, 0.1, 1), 2, 2)))
plot(traineg, main = paste("r = ",  round(cor(traineg[,1], traineg[, 2]),2)), xlab = "x", ylab = "y" )

traineg <- rbind(mvrnorm(150, muA, matrix(c(1, 0.5, 0.5, 1), 2, 2)))
plot(traineg, main = paste("r = ",  round(cor(traineg[,1], traineg[, 2]),2)), xlab = "x", ylab = "y" ) 
traineg <- rbind(mvrnorm(150, muA, matrix(c(1, 0.005, 0.005, 1), 2, 2)))
plot(traineg, main = paste("r = ",  round(cor(traineg[,1], traineg[, 2]),2)), xlab = "x", ylab = "y" ) 
traineg <- rbind(mvrnorm(150, muA, matrix(c(1, -0.9, -0.1, 1), 2, 2)))
plot(traineg, main = paste("r = ",  round(cor(traineg[,1], traineg[, 2]),2)), xlab = "x", ylab = "y" )
traineg <- rbind(mvrnorm(150, muA, matrix(c(1, -1, -1, 1), 2, 2)))
plot(traineg, main = paste("r = ",  cor(traineg[,1], traineg[, 2])), xlab = "x", ylab = "y" )  

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, warning=FALSE, fig.align='center'----

#
      par(mfrow=c(2,2))
  plot(anscombe$x1, anscombe$y1, xlab = "", ylab = "", main = paste("r = ",round(cor(anscombe$x1, anscombe$y1), 2)), cex = 2, pch = 16, col = 4)

  plot(anscombe$x2, anscombe$y2, xlab = "", ylab = "", main = paste("r = ",round(cor(anscombe$x2, anscombe$y2), 2)), cex = 2, pch = 16, col = 4)

  plot(anscombe$x3, anscombe$y3, xlab = "", ylab = "", main = paste("r = ",round(cor(anscombe$x3, anscombe$y3), 2)), cex = 2, pch = 16, col = 4)

  plot(anscombe$x4, anscombe$y4, xlab = "", ylab = "", main = paste("r = ",round(cor(anscombe$x4, anscombe$y4), 2)), cex = 2, pch = 16, col = 4)


## ----fig.width=8,fig.height=8, results='hide',echo=FALSE, fig.align='center'----



ggplot(datasaurus_dozen,  aes(x=x, y=y, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)
#+   ggtitle("r = -0.06")

# cor(datasaurus_dozen_wide[, 1:2])[1,2]

## ----fig.width=6.5,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
datacorrelation <- read.csv("data/correlationexample.csv")
attach(datacorrelation)
op <- par(mfrow = c(1, 2))

plot(X1, Y1,  pch = 19)
plot(X2, Y2,  pch = 19)
par(op)

## ----echo = TRUE, fig.align='center'-------------------------------------
cor(X1,Y1)^2
summary(lm(Y1 ~X1))[8]
cor(X2,Y2)^2
summary(lm(Y2 ~X2))[8]

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("Graphic1-9.jpg")

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
cigarette <- read.csv("data/Cigarette.csv")
attach(cigarette)
op <- par(mfrow = c(1, 1))
fit <- lm(carbon.monoxide ~ nicotine )
plot(nicotine , carbon.monoxide,  pch = 19 )
abline(fit, col = 2, lty = 1)
ID <- seq(1, dim(cigarette)[1])
text(nicotine , carbon.monoxide,labels = ID, cex= 0.7, pos = 2)
par(op)


## ----echo = TRUE, fig.align='center'-------------------------------------

summary(fit)

#studres(fit)


## ----fig.width=6.5,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(1, 2) )
plot(fit, which = c(1, 2))
par(op)

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----

op <- par(mfrow = c(1, 1) )
fit2 <- lm(carbon.monoxide[-3] ~ nicotine[-3] )
plot(nicotine , carbon.monoxide,  pch = 19)
abline(fit, col = 2, lty = 2)
abline(fit2, col = 2, lty = 1)
par(op)


## ----fig.width=6.5,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(1, 2) )
plot(fit2, which = c(1, 2))
par(op)

## ----fig.width=6.5,fig.height=10, results='hide',echo=FALSE, fig.align='center'----
fit3 <- lm(carbon.monoxide ~ sqrt(nicotine) )
fit4 <- lm(sqrt(carbon.monoxide) ~ sqrt(nicotine) )

fit5 <- lm(log(carbon.monoxide) ~ log(nicotine) )

op <- par(mfrow = c(4, 2) , cex = 0.8, mai = c(0.7,0.7,0.7,0.7), oma = c(0.9,0.9,0.9,0.9))

plot(nicotine , carbon.monoxide,  pch = 19, main =  bquote(R^2 ~ "=" ~ .(round(summary(fit)$r.squared,2))))
abline(fit, col = 2, lty = 2)
abline(fit2, col = 2, lty = 1)

plot(predict(fit), residuals(fit))
abline(h = 0, col = 2, lty = 1)


plot(sqrt(nicotine) , carbon.monoxide,  pch = 19, main =  bquote(R^2 ~ "=" ~ .(round(summary(fit3)$r.squared,2))))
abline(fit3, col = 2, lty = 1)


plot(predict(fit3), residuals(fit3))
abline(h = 0, col = 2, lty = 1)



plot(sqrt(nicotine) , sqrt(carbon.monoxide),  pch = 19, main =  bquote(R^2 ~ "=" ~ .(round(summary(fit4)$r.squared,2))))
abline(fit4, col = 2, lty = 1)


plot(predict(fit4), residuals(fit4))
abline(h = 0, col = 2, lty = 1)


plot(log(nicotine) , log(carbon.monoxide),  pch = 19, main =  bquote(R^2 ~ "=" ~ .(round(summary(fit5)$r.squared,2))))
abline(fit5, col = 2, lty = 1)


plot(predict(fit5), residuals(fit5))
abline(h = 0, col = 2, lty = 1)




par(op)
detach(cigarette)

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("deMoivre.jpg")
knitr::include_graphics("Legendre.jpg")
knitr::include_graphics("Gauss.jpg")
knitr::include_graphics("Laplace.jpg")
knitr::include_graphics("Adrain.jpg")
knitr::include_graphics("Galton.jpg")

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----
boxplot(father.son)

## ----echo = TRUE, fig.align='center'-------------------------------------
apply(father.son, 2, mean)

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----

plot(father.son, col = "grey40")
mylm <- (lm(son~father, data = father.son))
abline(mylm)

## ----echo = TRUE, fig.align='center'-------------------------------------
apply(father.son, 2, sd)

## ----fig.width=5, fig.height=5, results='hide',echo=FALSE, fig.align='center'----
plot(father.son, col = "grey40")
abline(mylm)

mylm2 <- (lm(father~son, data = father.son))
abline(-mylm2$coefficients[1]/mylm2$coefficients[2], 1/mylm2$coefficients[2])
# minimizes horizontaprediction errors
abline(apply(father.son, 2,  mean)[2] - apply(father.son, 2,  mean)[1], 1, col = 3)
# sons heigths equal the fathers

## ----fig.width=6.5,fig.height=3.8, results='hide',echo=FALSE, fig.align='center'----
par(mfrow=c(1,2), oma =  c(0.1, 0.1, 0.1, 0.1))
mylm22 <- (lm(son~father, data = father.son2))
plot(father.son2, col = "grey", xlim = c(-10, 10) , ylim = c(-10, 10))
abline(mylm22)
segments(father.son2$father, father.son2$son, father.son2$father, predict(mylm22), col="red")
plot(father.son2, col = "grey", xlim = c(-10, 10) , ylim = c(-10, 10))

c <- cov(father.son2)
decomp <- eigen(c)
a1 <- decomp$vectors[2,1]/decomp$vectors[1,1]
abline(0, a1,  col="red")
pcs <- (as.matrix(father.son2)%*% decomp$vectors) #rotate
pcs[,2]<-0 # remove second
denoised <- (pcs %*%  solve(decomp$vectors))
segments( father.son2$father,father.son2$son, denoised[ ,1],denoised[, 2],  col="red")

## ----fig.width=6.5,fig.height=3.8, results='hide',echo=FALSE, fig.align='center'----

par(mfrow=c(1,2), oma =  c(0.1, 0.1, 0.1, 0.1))
x <- seq(-1, 1, by = 0.01)
y <- x * 2
xsamp <- rnorm(length(x), x, 0.2)
ysamp <- rnorm(length(y), y, 0.8)
spectra <- as.data.frame(cbind(xsamp, ysamp))

plot(spectra, col = "grey", xlim = c(-4,4), ylim = c(-4,4))
mylm3 <- (lm(ysamp ~ xsamp, data = spectra))

abline(mylm3)
segments(spectra$xsamp, spectra$ysamp, spectra$xsamp, predict(mylm3), col="red")

c <- cov(spectra)
xbar <- apply(spectra, 2, mean)
plot(spectra, col = "grey", xlim = c(-4,4), ylim = c(-4,4))
decomp <- eigen(c)
a1 <- decomp$vectors[2,1]/decomp$vectors[1,1]
abline(0, a1)
pcs <- (as.matrix(spectra)%*% decomp$vectors) #rotate
pcs[,2]<-0 # remove second
denoised <- (pcs %*%  solve(decomp$vectors))
segments( spectra$xsamp,spectra$ysamp,denoised[ ,1],denoised[, 2], col = "red")


