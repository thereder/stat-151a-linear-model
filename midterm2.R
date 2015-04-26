library(mlbench)
setwd("/Users/hongzheli/Desktop/sp15/stat151a/lecture/")

### Question: whether ``clean air'' had an influence on house prices.?

####split by tax.



### Taking the logarithm or raising the variables to the power of something smaller than 
###one helps to reduce the asymmetry. This is due to the fact that lower values move further
###away from each other, whereas the distance between greater values is reduced by these 
###transformations.

# We can use F-statistics to compare a model with its sub model:
data(BostonHousing)
help(BostonHousing)
head(BostonHousing)
names(BostonHousing)
dim(BostonHousing)  ## 506 14
par(mfrow = c(4, 4))
attach(BostonHousing)
pairs(BostonHousing, panel= panel.smooth)
for(i in c(1:3, 5:13))
{
  hist(BostonHousing[,i], xlab = "", main = names(BostonHousing)[i], breaks = 500)
}

#rad and tax specific 2 parts.!!!!
# crim zn indus chas nox rm age dis rad tax ptratio b lstat 

which(crim <= 10 & crim >= 0) # 381 405 406 411 415 419 split into 2 parts.

which(zn >= 0 & zn <= 10)

which(age >= 90 & age <= 100)

which(rad == 24) # 357-488
which(tax == 666) # 357-488
which((BostonHousing$b >=350) & (BostonHousing$b <= 400))
which(ptratio >= 20 & ptratio <= 21)
which(indus >= 17 & indus <= 20)

## split data into 357-488, and rest!
par(mfrow = c(1, 1))

dataH = BostonHousing[357:488,]
dataL = BostonHousing[-c(which(medv == 50),357:488),]
dim(dataL)
dim(dataH)
dataL
attach(dataL)
dim(BostonHousing)
names(BostonHousing)
Lmodel = lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
plot(Lmodel$fitted,Lmodel$residuals, xlab = "fitted value", ylab = "residuals")
abline(h = 0, col = 'red')




#########outlier
par(mfrow = c(3,4))
for(i in c(1:3, 5:13))
{
  hist(dataL[,i], xlab = "", main = names(dataL)[i], breaks = 500)
}
par(mfrow = c(1,1))


which(crim > 3) # 143 144 156
which(nox > 0.8) # 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 160
which(tax > 600) # 346 347 348 349 350
which(ptratio >= 22) # 344 345
which(dataL$b <= 200) # 103 146 147 156 157
#probably outlier
probOut = c(143, 146, 147, 156, 157)

#leverage
ginf = influence(Lmodel)
ginf$hat
sum(ginf$hat)
plot(ginf$hat, type = "h")
n = nrow(dataL)
abline(h = 2*14/n)  ### 
hi.lev = which(ginf$hat > (2*14/n))
hi.lev  
# 103 143 146 147 153 155 156 157



#Standardized predicted residuals
par(mfrow = c(2,2))
plot(Lmodel$residual~Lmodel$fitted, xlab = "fitted", ylab = "residuals")
abline(h = 0, col = 'red')
abline(h = c(-7.5, 7.5), col = 'blue')
plot(rstandard(Lmodel)~Lmodel$fitted, xlab = "fitted", ylab = "rstandard")
abline(h = 0, col = 'red')
abline(h = c(-2.5, 2.5), col = 'blue')
plot(rstudent(Lmodel)~Lmodel$fitted, xlab = "fitted", ylab = "rstudent")
abline(h = 0, col = 'red')
abline(h = c(-2.5, 2.5), col = 'blue')

resH1 = which(abs(Lmodel$residuals) >= 7.5) # 158 178 363 
resH2 = which(abs(rstandard(Lmodel)) >= 2.5) # 8 158 178 181 221 226 249 332 363
resH3 = which(abs(rstudent(Lmodel)) >= 2.5) #  8 158 178 181 221 226 249 332 363 
resH1
resH2
resH3
par(mfrow = c(1,1))
#158 178 363 

#outliers using cooks.distance
cook = cooks.distance(Lmodel)
plot(cook, type = "h")
which(cook >= 0.03)
abline(h = 0.03, col = 'red')
#156 158 178 208 332

#156, 158, 178 is an outlier


##############variable selection.
library(leaps)
b = regsubsets(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat), dataL, nvmax= 13)
rs = summary(b)
rs$which
names(rs)
rs$adjr2
#13
#  [1] 0.7518620 0.7817385 0.8099910 0.8219186 0.8355450 0.8420060 0.8464191 0.8491786 0.8522901 0.8527790
#[11] 0.8536162 0.8545800 0.8546494
#lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
rs$cp
# > rs$cp
#[1] 257.28782 183.58333 114.30117  85.61607  52.92273  37.96680  28.10122  22.32407  15.72969  15.52949
#[11]  14.49493  13.16712  14.00000
#12
rs$bic
#[1] -495.1539 -536.8362 -582.2715 -600.9232 -624.9403 -634.6131 -640.0236 -641.7345 -644.4343 -640.7731
#[11] -637.9816 -635.5206 -630.8382
#9
#bic#lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
step(Lmodel, direction = "both")
#12
  #lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + tax + ptratio + log(dataL$b) + log(lstat))
#cross-validation
m1 = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
m2 = lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + tax + ptratio + log(dataL$b) + log(lstat), data = dataL)
m3 = Lmodel

cv.scores = rep(-999, 3)
cv.scores[1] = sum((m1$residuals^2)/((1 - influence(m1)$hat)^2))
cv.scores[2] = sum((m2$residuals^2)/((1 - influence(m2)$hat)^2))
cv.scores[3] = sum((m3$residuals^2)/((1 - influence(m3)$hat)^2))
cv.scores
##smallest!
m1Adjusted1 = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b[-156]) + log(lstat), dataL[-156,])
m1Adjusted2 = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b[-158]) + log(lstat), dataL[-158,])
m1Adjusted3 = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b[-178]) + log(lstat), dataL[-178,])
summary(m1Adjusted1) # 0.8524
summary(m1Adjusted2) # 0.8567
summary(m1Adjusted3) # 0.8568
selectedModel = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b[-c(158, 178)]) + log(lstat), dataL[-c(158, 178),])
summary(m1)
summary(m2)
summary(m3)
summary(selectedModel) #0.8612
#outlier:158,178

###diagnostic!!!!!!



###1:check Error Assumption
plot(selectedModel$fitted, selectedModel$residuals,xlab="Fitted Value", ylab="Residuals")
abline(h = 0, col = 'red')
#This plot does not suggest any non-constant variance or nonlinearity

###2: Normality
library('car')
qqPlot(residuals(selectedModel))
qqnorm(residuals(selectedModel))
qqline(residuals(selectedModel))
###QQplot looks quite good except for the beginning and ending, which indicates that residuals are approximately normal
### sharpiro test gives p-value 0.3077 which is greater than 0.05. 
###3: Correlated Errors
acf(residuals(selectedModel), lag.max = 100)
### there is no cutoffs.


#########outlier again!!!!


which(crim > 3) # 143 144 156
which(nox > 0.8) # 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 160
which(tax > 600) # 346 347 348 349 350
which(ptratio >= 22) # 344 345
which(dataL$b <= 200) # 103 146 147 156 157
#probably outlier
probOut = c(143, 146, 147, 156, 157)

#leverage
ginf = influence(selectedModel)
ginf$hat
sum(ginf$hat)
plot(ginf$hat, type = "h")
n = nrow(body)
abline(h = 2*14/n)  ### 
hi.lev = which(ginf$hat > (2*14/n))
hi.lev  
# 103 156 157

#Standardized predicted residuals
par(mfrow = c(2,2))
plot(selectedModel$residual~selectedModel$fitted, xlab = "fitted", ylab = "residuals")
abline(h = 0, col = 'red')
abline(h = c(-6.5, 6.5), col = 'blue')
plot(rstandard(selectedModel)~selectedModel$fitted, xlab = "fitted", ylab = "rstandard")
abline(h = 0, col = 'red')
abline(h = c(-2.5, 2.5), col = 'blue')
plot(rstudent(selectedModel)~selectedModel$fitted, xlab = "fitted", ylab = "rstudent")
abline(h = 0, col = 'red')
abline(h = c(-2.5, 2.5), col = 'blue')

resH1 = which(abs(selectedModel$residuals) >= 6.5) # 8 185 204 210 229 234 257 283 343 506
resH2 = which(abs(rstandard(selectedModel)) >= 2.5) # 8 185 204 210 234 257 283 343 506
resH3 = which(abs(rstudent(selectedModel)) >= 2.5) #  8 185 204 210 229 234 257 283 343 506 
resH1
resH2
resH3
par(mfrow = c(1,1))
#8 185 204 210 234 257 283 343 506



#Standardized predicted residuals
jack = rstudent(Lmodel)
#Is subject 162 an outlier?
p = 13
n = nrow(dataL)
qt(0.05/(n*2), (n- p -2))
jack[probOut]
jack[156]


#outliers using cooks.distance
cook = cooks.distance(selectedModel)
plot(cook, type = "h")
which(cook >= 0.04)
abline(h = 0.04, col = 'red')
#156 206 215 330 343

#343 is a prob outlier
#refit model
finalModel = lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b[-c(158, 178, 343)]) + log(lstat), dataL[-c(158, 178, 343),])
summary(finalModel)
#0.8617

### compare with full model

fullModel = lm(medv~., data = BostonHousing)
b = regsubsets(medv~., BostonHousing, nvmax = 13)
rs = summary(b)
rs$which
names(rs)
rs$adjr2
#11
#  [1] 0.7518620 0.7817385 0.8099910 0.8219186 0.8355450 0.8420060 0.8464191 0.8491786 0.8522901 0.8527790
#[11] 0.8536162 0.8545800 0.8546494
#lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
rs$cp
# > 11
## crim + zn + chas1 + nox + rm + dis + rad + tax + ptratio + b + lstat

rs$bic
#[1] -495.1539 -536.8362 -582.2715 -600.9232 -624.9403 -634.6131 -640.0236 -641.7345 -644.4343 -640.7731
#[11] -637.9816 -635.5206 -630.8382
#9
#bic#lm(medv ~ log(indus) + rm + age + dis + rad + tax + ptratio + log(dataL$b) + log(lstat))
step(fullModel, direction = "both")
## medv ~ crim + zn + chas1 + nox + rm + dis + rad + 
#tax + ptratio + b + lstat
#11
#lm(medv ~ log(crim) + zn + log(indus) + chas + nox + rm + age + dis + tax + ptratio + log(dataL$b) + log(lstat))
cmp = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + b + lstat, data = BostonHousing)
summary(cmp)
