## ----results='hide', echo=FALSE------------------------------------------

#function to draw normal curves on regression lines
plot.norm<-function(x,mymodel,mult=1,sd.mult=3,mycol='gray60',howmany=150) {
  yvar<-mymodel$model[,1]
  xvar<-mymodel$model[,2]
  sigma<-summary(mymodel)$sigma
  stick.val<-rep(xvar[x],howmany)+mult*dnorm(seq(predict(mymodel)[x]-sd.mult*sigma, predict(mymodel)[x]+sd.mult*sigma, length=howmany), mean=predict(mymodel)[x],sd=sigma)
  steps<-seq(predict(mymodel)[x]-sd.mult*sigma,predict(mymodel)[x]+sd.mult*sigma,length=howmany)
  for(i in 1:length(steps)) {
    curstep<-steps[i]
    curdnorm<-stick.val[i]
    segments(xvar[x],curstep,curdnorm,curstep,lty=1,col=mycol)
  }
}


## ----results='hide',echo=FALSE, fig.align='center'-----------------------
 heights <- read.table("data/heights.txt", header = TRUE)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
hist(heights$Dheight, xlim = c(55, 75),xlab = "daughter height (inches)", main = "")


## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
plot(heights$Mheight,heights$Dheight, xlim = c(55, 75), ylim = c(55, 75),xlab = "mother height (inches)", ylab = "daughter height (inches)")
abline(0,1, lty=2)

## ----fig.width=5,fig.height=5, results='hide',echo=FALSE, fig.align='center'----
plot(heights$Mheight,heights$Dheight,xlim = c(55, 75), ylim = c(55, 75), xlab = "mother height (inches)", ylab = "daughter height (inches)")
abline(0,1, lty=2)
abline(lm(heights$Dheigh~heights$Mheight),lty = 1)
#lo <- loess(heights$Dheigh~heights$Mheight, span = 0.1)
#lines(predict(lo), col='red', lty = 3)

## ----fig.width=4.5,fig.height=3.5, results='hide',echo=FALSE, fig.align='center'----
bacteria <- read.csv("data/bacteria.csv")
qplot(Temp,Count, data = bacteria) +
geom_point(shape=1, position=position_jitter(width=0.1,height=.1))+ geom_smooth(method=lm,se=FALSE)

#ggsave.latex(p, filename = paste(rfigs,"/bacteria.pdf",sep=""), width = 4.5, height = 3, caption = NULL,floating=FALSE)

## ----fig.width=4.5,fig.height=3.5, results='hide',echo=FALSE, fig.align='center'----

qplot(Temp,log(Count), data = bacteria) +
geom_point(shape=1, position=position_jitter(width=0.1,height=.1))+ geom_smooth(method=lm,se=FALSE)

#ggsave.latex(p, filename = paste(rfigs,"/logbacteria2.pdf",sep=""), width = 4.5, height = 3, caption = NULL,floating=FALSE)

## ----results='hide',echo=FALSE, fig.align='center'-----------------------

library(Sleuth3)
attach(ex0915)
pairs(ex0915[,c(2,3,1)])

## ----results='hide',echo=FALSE, webgl=TRUE-------------------------------
plot3d(Rainfall, Year, Yield, col="blue")

## ----results='hide',echo=FALSE, fig.align='center'-----------------------
driving <- read.table("data/fuel2001.txt", header = TRUE)

## ----fig.width=6,fig.height=6, results='hide',echo=FALSE, fig.align='center'----
driving <- driving[, -c(5,8)]
pairs(driving[, c(2,1,3,4,5,6)])

## ----fig.width=6,fig.height=6,results='hide',echo=FALSE, fig.align='center'----
driving2 <- driving
driving2$Drivers <- driving2$Drivers/driving2$Pop
driving2$FuelC <- driving2$FuelC/driving2$Pop
driving2$Miles <- log(driving2$Miles,2)
names(driving2)[1]<- "Drivers2"
names(driving2)[2]<- "FuelC2"
names(driving2)[4]<- "Miles2"
pairs(driving2[, c(2,1,3,4,5,6)])

## ------------------------------------------------------------------------
library(MASS)
parcoord(driving2[, c(2,6, 1,3,4,5)])

## ----echo=FALSE, results='asis', fig.align='center'----------------------
Temp <-  c(28,  28,  32.5,	39,	45.9,	57.8,	58.1,	62.5)
 Fuel <- c(12.4,	11.7,	12.4,	10.8,	9.4,	9.5,	8,	7.5)
knitr::kable(cbind(Temp, Fuel))

## ----fig.width=5,fig.height=4,results='hide',echo=FALSE, fig.align='center'----

qplot(Temp,Fuel)  + geom_smooth(method=lm,  se=FALSE)

#ggsave.latex(p, filename = paste(rfigs,"/fueltemp.pdf",sep=""), width = 4.5, height = 3, caption = NULL,floating=FALSE)


## ----fig.width=5,fig.height=4,results='hide',echo=FALSE, fig.align='center'----

qplot(Temp,Fuel)  + geom_smooth(method = loess, alpha = 0.1,  se=FALSE)

#ggsave.latex(p, filename = paste(rfigs,"/fueltemp2.pdf",sep=""), width = 4.5, height = 3, caption = NULL,floating=FALSE)


