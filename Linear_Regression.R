
library(dplyr)

## ---- Read in the dataset----------------------------------------
prices <- read.csv("dataset.csv", header = T)
prices<-prices[-c(1:3)]
install.packages('fastDummies')
library('fastDummies')
prices <- dummy_cols(prices, select_columns = c('fueltype','aspiration','doornumber','carbody',
'drivewheel','enginelocation','enginetype','cylindernumber','fuelsystem'),remove_selected_columns = TRUE)

head(prices)
summary(prices)
str(prices)
cor(prices)
cor(prices,prices$price)



## ---- Correlation Matrix Visualization---------------------------------------
library(corrplot)
res <- cor(prices)
round(res,2)
cor.vis <- round(res,2)
corrplot(cor.vis, method = "number")
cor.vis
# improved correlation matrix
corrplot(cor(prices),
         method = "number",
         type = "upper" # show only upper side
)

## ---- Scatter Plot Matrix---------------------------------------
colnames(prices)

pairs(prices[,c(2,3,4,14)])
pairs(prices[,c(5,6,7,14)])
pairs(prices[,c(8,9,10,14)])
pairs(prices[,c(11,12,13,14)])
pairs(prices[,c(15,16,14)])
pairs(prices[,c(17,18,19,14)])
pairs(prices[,c(20,21,22,14)])
pairs(prices[,c(23,24,25,14)])
pairs(prices[,c(26,27,28,14)])
pairs(prices[,c(29,30,31,14)])
pairs(prices[,c(32,33,34,14)])
pairs(prices[,c(35,36,37,14)])
pairs(prices[,c(38,39,40,14)])
pairs(prices[,c(41,42,43,14)])
pairs(prices[,c(44,45,46,14)])
pairs(prices[,c(47,48,49,14)])
pairs(prices[,c(50,51,52,14)])

 
## ---- First Model---------------------------------------
model_1 <- lm(price ~ carlength + wheelbase + curbweight + carheight +	carwidth + cylindernumber_four+drivewheel_rwd+fuelsystem_mpfi+
                         + enginesize + boreratio + horsepower + compressionratio + stroke + highwaympg + citympg + peakrpm ,data = prices)
summary(model_1)
library(car)
residualPlot(model_1)

?boxplot

## ---- Apply Transformations --------------------------------------

boxplot(prices$highwaympg, main="highwaympg")
boxplot(sqrt(prices$highwaympg), main="sqrt(highwaympg)")

boxplot(prices$citympg, main="citympg")
boxplot(sqrt(prices$citympg), main="sqrt(citympg)")


boxplot(prices$price, main="price")
boxplot(log(prices$price), main="log(price)")


boxplot(prices$carlength, main="carlength")
boxplot(log(prices$carlength), main="log(carlength)")


boxplot(prices$enginesize, main="enginesize")
boxplot(log(prices$enginesize), main="log(enginesize)")

boxplot(prices$boreratio, main="boreratio")
boxplot(log(prices$boreratio), main="log(boreratio)")

boxplot(prices$horsepower, main="horsepower")
boxplot(log(prices$horsepower), main="log(horsepower)")


boxplot(prices$highwaympg, main="highwaympg")
boxplot(log(prices$highwaympg), main="log(highwaympg)")


boxplot(prices$peakrpm, main="peakrpm")
boxplot(log(prices$peakrpm), main="log(peakrpm)")



model_2 <-lm(log(price) ~ log(prices$carlength) + wheelbase + curbweight + carheight +	carwidth + log(enginesize) 
             + boreratio + log(horsepower) + compressionratio + stroke + sqrt(citympg) + log(peakrpm)+ 
               cylindernumber_four+drivewheel_rwd+fuelsystem_mpfi, data = prices)
summary(model_2)
library(car)
library(lmtest)
residualPlot(model_2)
residualPlots(model_2)
bptest(model_2)
qqPlot(model_2)
residuals_2<- residuals(model_2)
residuals_2<- scale(residuals_2,center = TRUE, scale = TRUE)
ks.test(residuals_2, "pnorm")
vif(model_2)

model_3 <-lm(log(price) ~  carheight +	carwidth + log(enginesize) + log(horsepower) + compressionratio + stroke + sqrt(citympg) + 
               cylindernumber_four+drivewheel_rwd+fuelsystem_mpfi, data = prices)

summary(model_3)
library(car)
library(lmtest)
residualPlot(model_3)
residualPlots(model_3)
bptest(model_3)
qqPlot(model_3)
residuals_2<- residuals(model_3)
residuals_2<- scale(residuals_2,center = TRUE, scale = TRUE)
ks.test(residuals_2, "pnorm")
vif(model_3)


## ---- histogram for Normality---------------------------------------
library(MASS)
sresid <- studres(model_3)
#remove NaN's from the residuals
sresid <- na.omit(sresid)
hist(sresid, freq=FALSE,
     main="Distribution of Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


library(MASS)
stepAIC(model_2)
stepAIC(model_2, direction = "both")

stepAIC(model_3)
stepAIC(model_3, direction = "both")

install.packages("leaps")
library(leaps)
reg.subset <- regsubsets(log(price) ~  carheight +	carwidth + log(enginesize) + log(horsepower) + compressionratio + stroke + sqrt(citympg) + 
                           cylindernumber_four+drivewheel_rwd+fuelsystem_mpfi, data = prices)
plot(reg.subset) 
plot(reg.subset, scale = "adjr2") 
plot(reg.subset, scale = "r2") 

model_4 <-lm(log(price) ~  carheight +	carwidth + log(enginesize) + log(horsepower) + compressionratio + sqrt(citympg) + 
               cylindernumber_four+drivewheel_rwd, data = prices)

summary(model_4)
library(car)
library(lmtest)
residualPlot(model_4)
residualPlots(model_4)
bptest(model_4)
qqPlot(model_4)
residuals_2<- residuals(model_4)
residuals_2<- scale(residuals_2,center = TRUE, scale = TRUE)
ks.test(residuals_2, "pnorm")
library(MASS)
sresid <- studres(model_4)
#remove NaN's from the residuals
sresid <- na.omit(sresid)
hist(sresid, freq=FALSE,
     main="Distribution of Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

vif(model_4)

library(MASS)
stepAIC(model_4)
stepAIC(model_4, direction = "both")


