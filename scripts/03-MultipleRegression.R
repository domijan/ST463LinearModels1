## ----echo = FALSE--------------------------------------------------------
Temp <-  c(28,  28,  32.5,	39,	45.9,	57.8,	58.1,	62.5)
Fuel <- c(12.4,	11.7,	12.4,	10.8,	9.4,	9.5,	8,	7.5)
Chill <- c(18, 14, 24, 22, 8, 16, 1, 0)

kable(data.frame(Temp = Temp, Fuel = Fuel, Chill = Chill))

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----


scatterplot3d(Temp, Chill, Fuel)


## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
x1 <- runif(100)
x2 <- rbinom(100, 1, 0.5)
y <- 0.2 + 2 * x2 + 2 * x1 + rnorm(100, 0, 0.3)
plot(x1, y)

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
x1 <- runif(100)
x2 <- rbinom(100, 1, 0.5)
y <- 0.2 + 2 * x2 + 2 * x1 + 3 * x1 * x2 + rnorm(100, 0, 0.3)
plot(x1, y)

## ----fig.width=4,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
x1 <- runif(100)
y <- 0.2 + 2 * x1 + 5 * x1^2 + rnorm(100, 0, 0.3)
plot(x1, y)

## ----results='hide',echo=FALSE, webgl=TRUE-------------------------------


# plot3d(x1, x1^2, y)


x <- seq(0,1, by = 0.1)
x2 <- x^2
f <- function(x,x2) { r <-  0.2 + 2 * x + 5 * x2 }
z <- outer(x, x2, f)
persp3d(x, x2, z,  col = "light blue", xlab = "X1", ylab = "X1^2", zlab = "Y")
points3d(x1,x1^2,y,col="red")


## ----fig.width=6.5,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
cigarette <- read.csv("data/Cigarette.csv")
attach(cigarette)
op <- par(mfrow = c(1, 2) )
fit <- lm(carbon.monoxide ~ nicotine )
plot(nicotine , carbon.monoxide,  pch = 19)
abline(fit, col = 2, lty = 1)
fit2 <- lm(carbon.monoxide ~ weight )
plot(weight , carbon.monoxide,  pch = 19)
abline(fit2, col = 2, lty = 1)
par(op)


## ----echo = TRUE, fig.align='center'-------------------------------------

summary(fit)

## ----echo = TRUE, fig.align='center'-------------------------------------
summary(fit2)


## ----fig.width=6.5,fig.height=4, results='hide',echo=FALSE, fig.align='center'----
op <- par(mfrow = c(1, 2) )
scatterplot3d(weight ,nicotine, carbon.monoxide, pch = 19)
scatterplot3d(weight ,nicotine, carbon.monoxide, pch = 19, angle = 220)
par(op)

## ----fig.width=6,fig.height=6, results='hide',echo=FALSE, fig.align='center'----
pairs(cbind(carbon.monoxide, weight, nicotine))

## ----echo = TRUE, fig.align='center'-------------------------------------
fit3 <- lm(carbon.monoxide ~ weight + nicotine)
summary(fit3)

## ----echo = TRUE, fig.align='center'-------------------------------------
nicotine.sq <- nicotine^2
fit4 <- lm(carbon.monoxide ~ nicotine + nicotine.sq)
summary(fit4)

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----
plot(nicotine , carbon.monoxide,  pch = 19, main =  bquote(R^2 ~ "=" ~ .(round(summary(fit4)$r.squared,2))))
curve(-1.784 + 20.111 * x + -3.730 * x^2, 0, 2,add = TRUE , col = 2, lty = 1)

## 
## Let $\hat{\sigma}^2 = \frac{\mbox{SSE}}{n-p}$. When the model assumptions hold:

## $\mbox{SST} = \mbox{SSR} + \mbox{SSE}$

## ----echo = FALSE--------------------------------------------------------
SOURCE <- c("Regression", "Error", "Total")
df <- c("p-1", "n-p", "n-1")
SS <- c("SSR", "SSE", "SST")
MS <- c("MSR = SSR/(p-1)", "MSE = SSE/(n-p)", "")
Fstat <- c("MSR/MSE", "", "")

kable(data.frame(SOURCE=SOURCE, df = df, SS = SS, MS = MS, F = Fstat))

## ----echo = FALSE--------------------------------------------------------
SOURCE <- c("Group", "Error", "Total")
df <- c("g-1", "n-g", "n-1")
SS <- c("SSG", "SSE", "SST")
MS <- c("MSG = SSG/(g-1)", "MSE = SSE/(n-g)", "")
Fstat <- c("MSG/MSE", "", "")

kable(data.frame(SOURCE=SOURCE, df = df, SS = SS, MS = MS, F = Fstat))

## ----echo = FALSE--------------------------------------------------------
SOURCE <- c("Regression", "Error", "Total")
df <- c("p", "n-p", "n")
SS <- c("SSR", "SSE", "SST")


kable(data.frame(SOURCE=SOURCE, df = df, SS = SS))

## ----fig.width=3,fig.height=3, results='hide',echo=FALSE, fig.align='center'----
coffee.data <- read.csv("data/coffee.csv")
boxplot(y ~ x, data = coffee.data)


## ----echo = TRUE, fig.align='center'-------------------------------------



#Tell R this is a categorical variable
coffee.data$x <- as.factor(coffee.data$x)

fit.oneway.anova <- aov(y~x, data = coffee.data) 
summary(fit.oneway.anova)
anova(fit.oneway.anova)
model.tables(fit.oneway.anova, "means")
# plot(fit.oneway.anova) #diagnostic plots
coefficients(fit.oneway.anova)
#TukeyHSD(fit.oneway.anova, "x", ordered = TRUE)
#plot(TukeyHSD(fit.oneway.anova, "x"))


fit.coffee <- lm(y~x, data = coffee.data)
summary(fit.coffee)
anova(fit.coffee)

# or using dummy variables

d1 <- as.numeric(coffee.data$x == 1)
d2 <- as.numeric(coffee.data$x == 2)
d3 <- as.numeric(coffee.data$x == 3)

fit.coffee.dummy <- lm(coffee.data$y ~ d1 +d2)
summary(fit.coffee.dummy)

#no intercept
fit.coffee.dummy2 <- lm(coffee.data$y ~ d1 +d2 +d3 - 1)
summary(fit.coffee.dummy2)


