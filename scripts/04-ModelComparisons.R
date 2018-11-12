## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
steamdata <- read.csv("data/Steam.csv")
attach(steamdata)
pairs(cbind(STEAM, TEMP, INV, PROD))
#parcoord(cbind(STEAM, TEMP,STEAM, INV, PROD))

## ---- fig.align='center'-------------------------------------------------
modelA <- lm(STEAM ~ TEMP)
modelB <- lm(STEAM ~ TEMP + INV + PROD)




## ------------------------------------------------------------------------
summary(modelA)
summary(modelB)

## ------------------------------------------------------------------------
anova(modelA)
anova(modelB)

## ---- fig.align='center'-------------------------------------------------
anova(modelA, modelB)

## ----echo = FALSE--------------------------------------------------------
SOURCE <- c("TEMP", "INV", "PROD")
df <- c("1", "1", "1")
SS <- c("45.592", "9.292", "0.004")
Notation <- c("SSR(B|A)", "SSR(C|B)", "SSR(D|C)")


kable(data.frame(SOURCE=SOURCE, df = df, seqSS = SS, Notation = Notation))

## ---- fig.align='center'-------------------------------------------------

modelB <- lm(STEAM ~ TEMP + INV + PROD)

anova(modelB)
anova(lm(STEAM ~ PROD  + INV + TEMP))

#library(car)
Anova(modelB, type= 2)


## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----

x <- rep(seq(1, 5, by = 1),5)
y <- rep(seq(1, 5, by = 1),5) + rnorm(25,0,0.5)


plot(x,y, xlim = c(0,6), ylim = c(0,7), xlab = "", ylab = "")

points(x,y)
abline(lm(y~x))


## ----fig.width = 6,fig.height = 3.5, results='hide',echo=FALSE, fig.align='center'----
data.voltage <- read.csv("data/Voltage.csv")

attach(data.voltage)
op <- par(mfrow = c(1, 2) )
plot(VOLTAGE, TIME)
plot(VOLTAGE, log(TIME))
voltage.fit <- lm(log(TIME) ~ VOLTAGE)
abline(voltage.fit, col = 2)
par(op)

## ---- fig.align='center'-------------------------------------------------
anova(lm(log(TIME)~VOLTAGE))
anova(lm(log(TIME)~as.factor(VOLTAGE)))

## ----fig.width=6,fig.height=6,  results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(2, 2) )
plot(voltage.fit)
par(op)
detach(data.voltage)

## ---- fig.align='center'-------------------------------------------------
fit1 <- lm(STEAM ~ TEMP + INV)
fit2 <- lm(PROD ~ TEMP + INV)
summary(lm(resid(fit1)~ resid(fit2)))

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
plot(resid(fit2), resid(fit1))
abline(lm(resid(fit1)~ resid(fit2)), col = 2)

## ----fig.width=6,fig.height=6, results='hide',echo=FALSE, fig.align='center'----
avPlots(modelB) # in library(car)

