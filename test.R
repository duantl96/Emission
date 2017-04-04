library(dplyr)
library(glmnet)
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
coef(lasso, s = "lambda.1se")
coef(lasso, s = "lambda.min")

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
