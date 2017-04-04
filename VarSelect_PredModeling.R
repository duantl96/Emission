library(dplyr)
library(reshape2)
library(MASS)
library(leaps)
library(glmnet)
require(caret)
library(randomForest)
library(ggplot2)

# Read in Data
e = read.csv("FULL.csv",stringsAsFactors = F)

# Clean-up
e = e %>%
  mutate(Dept.Name = as.factor(Dept.Name),
         AcctgMonth = as.Date(AcctgMonth,"%m/%d/%Y"),
         PrimaryUse = as.factor(PrimaryUse))

# Separate train and test subsets
train = subset(e,e$AcctgMonth < as.Date("7/1/2015","%m/%d/%Y"))
test = subset(e,e$AcctgMonth >= as.Date("7/1/2015","%m/%d/%Y"))

# Stepwise ----------------------------------------------------------------
# AIC (the lower the better)
fit.A <- lm(emission.sum ~ dept.total.area + PrimaryUse 
          + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
          + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
          + sum.n_course., data = e)
step.A <- stepAIC(fit.A, direction="both")
#step.A$anova # dept.total.area, PrimaryUse, TAVG, TMAX, TMIN, CLDD, sum.n_course.

# Adjusted R^2 (higher) and BIC (lower)
leaps=regsubsets(emission.sum ~ dept.total.area + PrimaryUse 
                 + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
                 + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
                 + sum.n_course., data = e, nbest=5)
plot(leaps, scale="adjr2") # dept.total.area, PrimaryUse, CLDD, DX32, sum.n_course.
plot(leaps, scale = "bic") # dept.total.area, PrimaryUse, sum.n_course.

# LASSO -------------------------------------------------------------------
X = model.matrix(emission.sum ~ dept.total.area + PrimaryUse 
                 + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
                 + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
                 + sum.n_course. -1, data = train)
Y = train$emission.sum

# Fit a lasso model
lasso = cv.glmnet(X, Y)

# Plot mean-squared error as a function of the shrinkage parameter
plot(lasso)

# Find the coefficients of the optimal model
coef(lasso, s = "lambda.1se") #dept.total.area, PrimaryUse ONLY
coef(lasso, s = "lambda.min") #dept.total.area, PrimaryUse, DX32, sum.n_course

# Find the optimal value of the shrinkage parameter
lasso$lambda.min #1.33
lasso$lambda.1se #7.81

# Predict and save fitted values from the lasso model
la.X = model.matrix(emission.sum ~ dept.total.area + PrimaryUse 
                 + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
                 + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
                 + sum.n_course. -1, data = test)

lasso.fitted.lse = predict(lasso, newx = la.X, s = "lambda.1se",type="response")
lasso.fitted.min = predict(lasso, newx = la.X, s = "lambda.min",type="response")

# Plot the predicted values versus the truth
plot_data = data.frame(test$emission.sum[1:318], lasso.fitted.lse, lasso.fitted.min)
names(plot_data) = c("TrueEmission", "Lasso.lse", "Lasso.min")
p = ggplot(plot_data, aes(y = TrueEmission))
p + geom_point(aes(x = Lasso.lse), color="Blue") +
  geom_point(aes(x = Lasso.min), color="red") +
  geom_abline(slope=1) + xlab("Fitted Value")



# Elastic Net -------------------------------------------------------------
#https://quantmacro.wordpress.com/2016/04/26/fitting-elastic-net-model-in-r/
lambda.grid <- 10^seq(2,-2,length = 100)
alpha.grid <- seq(0,1,length = 10)

trnCtrl = trainControl(
  method = "repeatedCV",
  number = 10,
  repeats = 5
)

srchGrd = expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)

set.seed(1)
en.train <- train(emission.sum ~ dept.total.area + PrimaryUse 
                  + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
                  + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
                  + sum.n_course., data = train,
                  method = "glmnet",
                  tuneGrid = srchGrd,
                  trControl = trnCtrl,
                  standardize = TRUE,
                  maxit = 1000000)

plot(en.train)

en.train$bestTune # alpha = 1, lambda = 1.668

enet = en.train$finalModel

coef(enet, s = en.train$bestTune$lambda) # dept.total.area, PrimaryUse, DX32, sum.n_course.

# Predict with fitted elastic net model (not working)
enet.X = test %>%
  select(dept.total.area, PrimaryUse 
        , TAVG, TMAX, TMIN, HTDD, CLDD, EMXT, EMNT 
        , DX90, DX32, DT32, PRCP, EMXP, DP01, DP05, DP10
        , sum.n_course.,emission.sum)
enet.X$emission.sum = NULL
enet.fitted = predict(enet, newx = data.matrix(enet.X), type = "response")

# Random Forest ----------------------------------------------------------
# Fit a random forest
rf = randomForest(emission.sum ~ dept.total.area + PrimaryUse 
                  + TAVG + TMAX + TMIN + HTDD + CLDD + EMXT + EMNT 
                  + DX90 + DX32 + DT32 + PRCP + EMXP + DP01 + DP05 + DP10
                  + sum.n_course., data = train, importance = TRUE)

# Plot the mean squared error
plot(rf)

# Find the variable importance
importance(rf) #dept.total.area, PrimaryUse, sum.n_course

# Save the predicted values
rf.fitted = predict(rf, newdata = test)

# Plot the predicted values versus the truth
plot_data = data.frame(test$emission.sum, rf.fitted)
names(plot_data) = c("TrueEmission", "RandomForest")
p = ggplot(plot_data, aes(y = TrueEmission))
p + geom_point(aes(x = RandomForest), color="green") +
  geom_abline(slope=1) + xlab("Fitted Value")


# Compare Predictions -------------------------------------------------------------
# Plot predicted values of all methods versus the truth (Random Forest had the best performance)
plot_data = data.frame(test$emission.sum[1:318], lasso.fitted.lse, lasso.fitted.min, rf.fitted[1:318])
names(plot_data) = c("TrueEmission", "Lasso.lse", "Lasso.min", "RandomForest")
pd.melt <- melt(plot_data, id.vars = "TrueEmission", variable.name = "Method", 
                       value.name = "Prediction")
ggplot(pd.melt, aes(x = Prediction, y = TrueEmission, color = Method)) + 
  geom_point() + geom_abline(slope=1)

