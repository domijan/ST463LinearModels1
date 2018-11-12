## ----fig.width=6,fig.height=6, results= 'hide', echo = FALSE, fig.align='center'----
split.screen(c(2,1))
split.screen(c(1,2), screen = 1)
split.screen(c(1,2), screen = 2)
screen(3)
x <- runif(50)
y <- runif(50) - 0.5
plot(x,y, ylab = expression(e[i]), xlab = expression(paste(hat(y)[i], " or ", X[ij])), main  = "ideal pattern")
abline(h = 0)
screen(4)
x <- runif(50)
y <- (runif(50) - 0.5) * x
plot(x,y, ylab = expression(e[i]), xlab = expression(paste(hat(y)[i], " or ", X[ij])), main  = "non-constant variance")
abline(h = 0)
screen(5)
x <- runif(50)
y <- sin(x*pi*2) + (runif(50) - 0.5)
plot(x,y, ylab = expression(e[i]), xlab = expression(paste(hat(y)[i], " or ", X[ij])), main  = "curvature" )
#expression(paste("curvature suggests that ", E(e[i])!=0, " thus ",E(epsilon[i])!=0))
abline(h = 0)
screen(6)
x <- runif(50)
y <- sin(x*pi*2) + (runif(50) - 0.5)*x*2.5
plot(x,y, ylab = expression(e[i]), xlab = expression(paste(hat(y)[i], " or ", X[ij])), main  = "curvature, non-const. var.")
abline(h = 0)

## ----fig.width=4,fig.height=4, results='hide', echo = FALSE, fig.align='center'----

bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))
bivn.kde <- kde2d(bivn[,1], bivn[,2], h = 3, n = 100)
bivn2 <- mvrnorm(50, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))

#X<- bivn2 
#hat.mat <- X %*% qr.solve(t(X) %*% X) %*% t(X) 

#plot(bivn2, col = 0, xlab = "X1", ylab = "X2");contour(hat.mat, add = T,  nlevels = 5)

plot(bivn, col = 0, xlab = "X1", ylab = "X2");contour(bivn.kde, add = T,  nlevels = 5, drawlabels = FALSE)

points(apply(bivn, 2,  mean)[1], apply(bivn, 2,  mean)[2], pch = "M", col = 2, cex = 1.2)
points(bivn2)

## ----results='hide',echo=FALSE, fig.align='center'-----------------------
brain.data <- read.csv("data/Species_brain.csv")
attach(brain.data)

## ----fig.width=6,fig.height=6 , results='hide',echo=FALSE, fig.align='center'----
pairs(brain.data[, -1])

## ------------------------------------------------------------------------
#library(car)
#library(MASS)
fit1 <- lm(BRAIN ~ BODY + GESTATION + LITTER)
summary(fit1)
# anova(fit1)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(2, 2) )
#plot(fit1)

#influence(fit1)$hat
#stdres(fit1)
plot(fit1, which = 1)
qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid
plot(influence(fit1)$hat, stdres(fit1), xlab = "leverage", ylab = "std residuals")
abline(h = 0, col = 2)
abline(v = 0.083, col = 2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(brain.data)-length(fit1$coefficients)-2))
plot(fit1, which = 4, cook.levels = cutoff)
D <- cooks.distance(fit1)
par(op)

## ------------------------------------------------------------------------
####################
# influence measures
####################


inflm.SR <- influence.measures(fit1)
int <- which(apply(inflm.SR$is.inf, 1, any))
cbind(brain.data[int ,],
      influence(fit1)$hat[int],# large leverage
      stdres(fit1)[int],# large std residual
      abs(D)[int]) # large Cook's D



## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
#avPlots(fit1)
op <- par(mfrow = c(2, 2), mai = c(0.8, 0.8, 0.1, 0.1), cex = 0.7 )
fitA <- lm(BRAIN ~ GESTATION + LITTER)
fitB <- lm(BODY ~ GESTATION + LITTER)

plot(resid(fitB), resid(fitA), xlab = "BODY | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)




fitA <- lm(BRAIN ~ LITTER + BODY)
fitB <- lm(GESTATION ~ LITTER + BODY)

plot(resid(fitB), resid(fitA), xlab = "LITTER | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)


fitA <- lm(BRAIN ~ GESTATION + BODY)
fitB <- lm(LITTER ~ GESTATION + BODY)

plot(resid(fitB), resid(fitA), xlab = "GESTATION | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)

par(op)

## ----fig.width=6,fig.height=6 , results='hide',echo=FALSE, fig.align='center'----
pairs(log(brain.data[, -1]))

## ---- fig.align='center'-------------------------------------------------
fit2 <- lm(log(BRAIN) ~ log(BODY) + log(GESTATION) + log(LITTER))
summary(fit2)

#anova(fit2)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(2, 2) )

plot(fit2, which = 1)
qqPlot(fit2, main="QQ Plot") #qq plot for studentized resid
plot(influence(fit2)$hat, stdres(fit2), xlab = "leverage", ylab = "std residuals")
abline(h = 0, col = 2)
abline(v = 0.083, col = 2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(brain.data)-length(fit2$coefficients)-2))
plot(fit2, which = 4, cook.levels = cutoff)
par(op)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
#avPlots(fit1)
op <- par(mfrow = c(2, 2), mai = c(0.8, 0.8, 0.1, 0.1), cex = 0.7 )
fitA <- lm(log(BRAIN) ~ log(GESTATION) + log(LITTER) )
fitB <- lm(log(BODY) ~ log(GESTATION) + log(LITTER) )

plot(resid(fitB), resid(fitA), xlab = "BODY | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)




fitA <- lm(log(BRAIN) ~ log(LITTER) + log(BODY) )
fitB <- lm(log(GESTATION) ~ log(LITTER) +log( BODY) )

plot(resid(fitB), resid(fitA), xlab = "LITTER | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)


fitA <- lm(log(BRAIN) ~ log(GESTATION) + log(BODY))
fitB <- lm(log(LITTER) ~ log(GESTATION) + log(BODY))

plot(resid(fitB), resid(fitA), xlab = "GESTATION | others", ylab = "BRAIN | others")
abline(lm(resid(fitA)~ resid(fitB)), col = 2)
text(resid(fitB), resid(fitA),labels = SPECIES, cex= 0.7, offset = 10)

par(op)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
rats.data <- read.table("data/rat.txt", header = TRUE)
attach(rats.data)
pairs(cbind(EndDose, BodyWt, LiverWt, Dose))


## ---- fig.align='center'-------------------------------------------------
#library(car)
#library(MASS)
fit1 <- lm(EndDose ~ BodyWt +  LiverWt + Dose, data = rats.data)
summary(fit1)
#anova(fit1)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(2, 2) )
#plot(fit1)

#influence(fit1)$hat
#stdres(fit1)
plot(fit1, which = 1)
qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid
plot(influence(fit1)$hat, stdres(fit1), xlab = "leverage", ylab = "std residuals")
abline(h = 0, col = 2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(rats.data)-length(fit1$coefficients) - 2))
plot(fit1, which = 4, cook.levels = cutoff)
D <- cooks.distance(fit1)
par(op)

## ---- fig.align='center'-------------------------------------------------
head(rats.data[sort(influence(fit1)$hat, 
decreasing = TRUE, index.return = TRUE)$ix,])

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----


remove <- rep(1, dim(rats.data)[1])
remove[which(influence(fit1)$hat == max(influence(fit1)$hat))] <- 19
pairs(cbind(EndDose, BodyWt, LiverWt, Dose), pch = remove )


## ---- fig.align='center'-------------------------------------------------

fit1 <- lm(EndDose ~ BodyWt +  LiverWt + Dose, data = rats.data[-3,])
summary(fit1)
#anova(fit1)

## ----fig.width=6,fig.height=6.5, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(2, 2) )
#plot(fit1)

#influence(fit1)$hat
#stdres(fit1)
plot(fit1, which = 1)
qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid
plot(influence(fit1)$hat, stdres(fit1), xlab = "leverage", ylab = "std residuals")
abline(h = 0, col = 2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(rats.data)-length(fit1$coefficients) - 2))
plot(fit1, which = 4, cook.levels = cutoff)
D <- cooks.distance(fit1)
par(op)
detach(rats.data)

