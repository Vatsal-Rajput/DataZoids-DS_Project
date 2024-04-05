library(rpart)
library(rpart.plot)
library(pracma)
library(scatterplot3d)
data <- read.csv('MarketingData')

graduation.data <- data$Income[data$Education=="Graduation"]

higherstudies.data.income <- data$Income[data$Education %in% c("Master", "PhD")]

t.test(graduation.data,higherstudies.data.income,var.equal=TRUE,conf.level=0.95)
#Null hypothesis - Mean of the income of people who have opted for higher studies is same as to those who have not

higherstudies.data <- data[data$Education %in% c("Graduation", "Master"),c(4,30)]

### MLE

summary(data$NumDealsPurchases)

hist(data$NumDealsPurchases)

exp.lik<-function(theta,y){
  logl<- sum(dexp(y,theta,log=TRUE))
  return(-logl)
}

lam <- optim(1,exp.lik,y=data$NumDealsPurchases,method="BFGS")$par

x <- seq(0,15,1)

hist(data$NumDealsPurchases)

# Creates a qqplot
par(mfrow=c(1,2))
qqplot(rexp(2216,lam),data$NumDealsPurchases,xlab = "Theoretical quantiles of rexp(lam)",ylab="Quantiles of our actual data",main = "Using MLE")
abline(0,1,col="red",lwd=3)
qqplot(rexp(2216,2),data$NumDealsPurchases,xlab = "Theoretical quantiles of rexp(lam)",ylab="Quantiles of our actual data",main = "Random lambda=2")
abline(0,1,col="red",lwd=3)

#Prop testing
table(higherstudies.data)
prop.test(c(964,309),c(1116,365),conf.level=0.95, correct=FALSE)

#orthogonal regression
res <- odregress(as.matrix(data[,c(11,16)]), as.matrix(data$Income))
res$coeff 


