## ----results='asis',echo=FALSE, fig.align='center'-----------------------
flowers.data <- read.csv("data/flowers.csv")
knitr::kable(head(flowers.data), rownames = NULL)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
attach(flowers.data)
plot(Intensity, Flowers, col = Timing)

## ------------------------------------------------------------------------
fit1 <- lm(Flowers ~ Intensity + Time)
summary(fit1)

## ------------------------------------------------------------------------
fit2 <- lm(Flowers ~ Intensity * Time)
summary(fit2)
model.matrix(fit2)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----


plot(Intensity, Flowers, col = Timing)

abline(a = coefficients(fit1)[1], b = coefficients(fit1)[2])
abline(a  = sum(coefficients(fit1)[-2]), b = coefficients(fit1)[2], col = 2)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
flying.data <- read.csv("data/flying.csv")
#attach(flying.data)
plot(flying.data$x1, flying.data$y, col = flying.data$Group, xlab = "X1"  , ylab = "Y")

## ---- fig.align='center'-------------------------------------------------
x1G <- flying.data$x1 * flying.data$G1
x2G <- flying.data$x1 * flying.data$G2
fitA <- lm(y ~ x1 + G1 + G2, data = flying.data)
summary(fitA)
anova(fitA)
fitB <- lm(y ~ x1 + G1 + G2 + x1G + x2G, data = flying.data)
summary(fitB)
anova(fitB)
anova(fitA, fitB)
fitC <- lm(y ~ x1, data = flying.data)
#summary(fitC)
#anova(fitC)

## ----fig.width=3.5,fig.height=3.5, results='hide',echo=FALSE, fig.align='center'----

plot(flying.data$x1, flying.data$y, col = flying.data$Group, xlab = "X1"  , ylab = "Y", main = "Model B")
abline(a  = coefficients(fitB)[1], b = coefficients(fitB)[2], col = 3)
abline(a  = sum(coefficients(fitB)[c(1,3)]), b = sum(coefficients(fitB)[c(2, 5)]), col = 1)
abline(a  = sum(coefficients(fitB)[c(1,4)]), b = sum(coefficients(fitB)[c(2, 6)]), col = 2)

plot(flying.data$x1, flying.data$y, col = flying.data$Group, xlab = "X1"  , ylab = "Y", main = "Model A")
abline(a  = coefficients(fitA)[1], b = coefficients(fitA)[2], col = 3)
abline(a  = sum(coefficients(fitA)[c(1,3)]), b = coefficients(fitA)[2], col = 1)
abline(a  = sum(coefficients(fitA)[c(1,4)]), b = coefficients(fitA)[2], col = 2)


plot(flying.data$x1, flying.data$y, col = flying.data$Group, xlab = "X1"  , ylab = "Y", main = "Model C")
abline(fitC)
#detach(flying.data)

