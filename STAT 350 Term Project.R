library(ISLR)
library(MASS)
library(leaps)
library(qpcR)
library(glmnet)

#Seed
set.seed(123)

#dataset
data(Carseats)

#scatterplot
plot(Carseats, col = "skyblue3")

#MLR model
fit = lm(Sales ~ ., data = Carseats)

#Response variable histogram
hist(Carseats$Sales, col = "lightblue", breaks = 50)

#Residuals plot
res = fit$residuals
res = scale(res)
plot(x = fit$fitted.values, y = res, xlab = 'Fitted values', ylab = 'standardized residuals')
abline(h = 0, col = 'red')

#Normal Q-Q Line plot
qqnorm(res)
abline(a = 0, b = 1, col = 'red')

summary(fit)
anova(fit)


#Leverages
library(faraway)
hatv <- hatvalues(fit)
locations <- row.names(Carseats)
halfnorm(hatv,labs=locations,ylab="Leverages")

#refitting without highest leverage point
Carseats2 <- Carseats[-311,]
mod2 <- lm(Sales ~., data=Carseats2)
hatv <- hatvalues(mod2)
predictors <- row.names(Carseats2)
halfnorm(hatv,labs=predictors,ylab="Leverages")

#cooks' distance
cooksd <- cooks.distance(fit)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/nrow(Carseats), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/nrow(Carseats), names(cooksd),""), col="red")

#studentized residuals
stud <- rstudent(fit)
stud[which.max(abs(stud))]


#DFFITS
df.fits <- as.data.frame(dffits(fit))
p <- length(fit$coefficients)-1
n <- nrow(Carseats)
threshold1 <- 2*sqrt(p/n)
threshold1

df.fits[order(-df.fits['dffits(fit)']), ]


#DFBETAS
df.betas <- as.data.frame(dfbetas(fit))
#calculate DFBETAS threshold value
threshold2 <- 2/sqrt(n)
threshold2

#Plotting all DFBETAS of predictors deemed important by summary(fit)
par(mfrow=c(3,1))
plot(df.betas$CompPrice, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$Income, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$Advertising, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$Price, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$ShelveLocGood, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$ShelveLocMedium, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

plot(df.betas$Age, type='h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

#variable inflation factor
vif(fit)

#New dataset by removing 2 outliers
Carseats2 <- Carseats[-c(357,358),]


#forward selection
mod.forward <- regsubsets(Sales~., data=Carseats2,
                          nbest=1, nvmax=9, method="forward")
summ.mod.forward <- summary(mod.forward)
summ.mod.forward

#backward selection
mod.backward <- regsubsets(Sales~., data=Carseats2,
                           nbest=1, nvmax=9, method="backward")
summ.mod.backward <- summary(mod.backward)
summ.mod.backward

#all-subset regression
mod.all <- regsubsets(Sales~., data=Carseats2,
                      nbest=1, nvmax=9, method="exhaustive")
summ.mod.all <- summary(mod.all)
summ.mod.all

#R^2 and AIC of various MLR models
mod2 <- lm(Sales ~. -Population -Urban, data=Carseats2)
mod2 <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, data=Carseats2)
summary(mod2)$adj.r.squared
extractAIC(mod2)

mod3 <- lm(Sales ~. -Population -Urban -Education,data=Carseats2)
mod3 <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age  + US, data=Carseats2)
summary(mod3)$adj.r.squared
extractAIC(mod3)

mod4 <- lm(Sales ~. -Population -Urban -US, data=Carseats2)
mod4 <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education, data=Carseats2)
summary(mod4)$adj.r.squared
extractAIC(mod4)

mod5 <- lm(Sales ~. -Population -Urban -Income, data=Carseats2)
mod5 <- lm(Sales ~ CompPrice  + Advertising + Price + ShelveLoc + Age + Education + US, data=Carseats2)
summary(mod5)$adj.r.squared
extractAIC(mod5)

mod6 <- lm(Sales ~. -Population -Urban -Education -US - Income, data=Carseats2)
mod6 <- lm(Sales ~ CompPrice + Advertising + Price + ShelveLoc + Age, data=Carseats2)
summary(mod6)$adj.r.squared
extractAIC(mod6)


#Press statistic of all previous models listed above
press1 <- PRESS(mod2)
press2 <- PRESS(mod3)
press3 <- PRESS(mod4)
press4 <- PRESS(mod5)
press5 <- PRESS(mod6)

press1$stat
press2$stat
press3$stat
press4$stat
press5$stat




get.folds = function(n, K) {
  ### Get the appropriate number of fold labels
  n.fold = ceiling(n / K) # Number of observations per fold (rounded up)
  fold.ids.raw = rep(1:K, times = n.fold) # Generate extra labels
  fold.ids = fold.ids.raw[1:n] # Keep only the correct number of labels
  
  ### Shuffle the fold labels
  folds.rand = fold.ids[sample.int(n)]
  
  return(folds.rand)
}

get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

#number of folds
K = 10

n = nrow(Carseats2)
folds = get.folds(n, K)


all.models = c("LS", "Ridge", "LASSO-Min", "LASSO-1se")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models


for(i in 1:K){
  # Split data
  data.train = Carseats2[folds != i,]
  data.valid = Carseats2[folds == i,]
  n.train = nrow(data.train)
  
  # Get response vectors
  Y.train = data.train$Sales
  Y.valid = data.valid$Sales
  
  #MLR and MSPEs
  fit.ls = lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls 
  
  #Ridge lambda tuning
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  fit.ridge = lm.ridge(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, lambda = lambda.vals, 
                       data = data.train)
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  matrix.valid.ridge = model.matrix(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, data = data.valid)
  pred.ridge = matrix.valid.ridge %*% coef.min
  
  #calculating and storing MSPEs
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  all.MSPEs[i, "Ridge"] = MSPE.ridge
  
  
  matrix.train.raw = model.matrix(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, data = data.train)
  matrix.train = matrix.train.raw[,-1]
  #LASSO lambda tuning
  all.LASSOs = cv.glmnet(x = matrix.train, y = Y.train)
  lambda.min = all.LASSOs$lambda.min
  lambda.1se = all.LASSOs$lambda.1se
  coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "coef")
  coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "coef")
  included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
                               type = "nonzero")
  included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
                               type = "nonzero")
  matrix.valid.LASSO.raw = model.matrix(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age + Education + US, data = data.valid)
  matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
  pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.min, type = "response")
  pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.1se, type = "response")
  
  #storing MSPEs
  MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
  all.MSPEs[i, "LASSO-Min"] = MSPE.LASSO.min
  
  MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
  all.MSPEs[i, "LASSO-1se"] = MSPE.LASSO.1se
}

par(mfrow=c(1,1))

#MSPE Boxplot
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))

#Calculate RMSPEs
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

# Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"))

#zooming in on good models
boxplot(all.RMSPEs, ylim = c(1, 1.03),
        main = paste0("CV RMSPEs over ", K, 
                      " folds (enlarged to show texture)"),
)

#variables included by LASSO
coef.LASSO.1se
coef.LASSO.min

#summary function of best MLR model
summary(fit.ls)
