rm(list=ls())
gc() # garbage collection

##Import Libraries ----------
library(fastDummies)
library(Amelia) # missmaps
library(gdata) # unknownToNA
library(dplyr)
library(tidyr)
library(mice) 
library(VIM) 
library(rpart) 
library(rpart.plot)
library(class) 
library(reshape2) 
library(ggplot2)
library(e1071) 
library(caret) 
library(randomForest) 
library(ranger)       
library(caret)       
library(h2o)          
library(stringr) 
library(pROC)
library(MLmetrics)

dat0 <- read.csv("D:/Classes/ISDS574/Project/train.csv",
                 header=T, na.strings=c(""," ","NA")) # transform whitespace to NA
#test <- read.csv("D:/Classes/ISDS574/Project/test.csv", 
#                 header=T, na.strings=c(""," ","NA")) # transform whitespace to NA

######## DATA CLEANING FUNCTION ########

clean = function(dat){
  
  # Revalue the output from ordinal to categorical
  dat$churn_risk_score[dat$churn_risk_score == -1 | dat$churn_risk_score == 1 |
                         dat$churn_risk_score == 2 | dat$churn_risk_score == 3] = 1
  dat$churn_risk_score[dat$churn_risk_score == 4 | dat$churn_risk_score == 5] = 0
  dat$churn_risk_score = as.factor(dat$churn_risk_score)
  table(dat$churn_risk_score)
  dat[which(dat$referral_id == 'xxxxxxxx'),]$joined_through_referral = 'No'
  dat[which(dat$referral_id != 'xxxxxxxx'),]$joined_through_referral = 'Yes'
  dat = subset(dat, select = -c(customer_id, Name, security_no,
                                referral_id, last_visit_time)) # Unrequired variables
  
  dat <- unknownToNA(x=dat, unknown= -999) # change outliers in days_since_last_login to NA
  dat <- unknownToNA(x=dat, unknown= "Unknown")# change unknown in gender data to NA 
  dat <- unknownToNA(x=dat, unknown= "?") # change ? data to NA in medium of operation
  dat <- unknownToNA(x=dat, unknown= "Error") # change error value to NA
  dat$avg_frequency_login_days = as.numeric(dat$avg_frequency_login_days)
  
  # Modify joining date
  
  dat$joining_date = as.Date(dat$joining_date, format = "%Y-%m-%d")
  joining_year = as.character(format(dat$joining_date, format = '%Y'))
  joining_qtr = as.character(ceiling(as.numeric(format(dat$joining_date, format = '%m'))/3))
  dat = cbind(dat, joining_year, joining_qtr)
  dat = select(dat, -joining_date)
  
  
  # Boxplot of numerical data
  
  par(mfrow = c(2, 3))
  boxplot(dat$age, main = 'Age', col = 'red')
  boxplot(dat$days_since_last_login, main = 'Days since last login', col = 'green')
  boxplot(dat$avg_time_spent, main = 'Average time spent', col = 'blue')
  boxplot(dat$avg_transaction_value, main = 'Average transaction value', col = 'yellow')
  boxplot(dat$avg_frequency_login_days, main = 'Average frequency login days', col = 'orange')
  boxplot(dat$points_in_wallet, main = 'Points in wallet', col = 'purple')
  par(mfrow = c(1, 1))
  
  mi = function(x){sum(is.na(x))/length(x)*100}
  apply(dat, 2, mi)
  
  # Fix negative values
  
  attach(dat)
  dat$avg_time_spent = replace(avg_time_spent, which(avg_time_spent<0), 
                               avg_time_spent[avg_time_spent<0]*-1)
  summary(points_in_wallet)
  points_in_wallet = replace_na(points_in_wallet, 3000) # avoid issues with NAs
  dat$points_in_wallet = replace(dat$points_in_wallet, which(points_in_wallet<0), 
                                 points_in_wallet[points_in_wallet<0]*-1)
  summary(avg_frequency_login_days)
  avg_frequency_login_days = replace_na(avg_frequency_login_days, 80) # avoid issues with NAs
  dat$avg_frequency_login_days = replace(dat$avg_frequency_login_days, which(avg_frequency_login_days<0), 
                                         avg_frequency_login_days[avg_frequency_login_days<0]*-1)
  detach(dat)
  
  dat
}

dat = clean(dat0)

######## CREATE TEST DATASET ########

set.seed(1)
id.train = sample(1:nrow(dat), nrow(dat)*.6)
id.test = setdiff(1:nrow(dat), id.train)
dat1 = dat[id.train,]
test = dat[id.test,]
test1 = na.omit(test)

######## MISSING VALUE ANALYSIS ########

missmap(dat1) 
pmiss = colMeans( is.na(dat1) ) # proportion of the missing value in columns
rmiss = rowMeans( is.na(dat1) )  # proportion of missing for each row
dat1 = dat1[rmiss < .15,]  # store rows that the missing proportion < 0.15
dat1 = dat1[,!pmiss > .25]  # store columns that the missing proportion < 0.25
mean(is.na(dat1$gender))
mean(is.na(dat1$preferred_offer_types))
dat1 = dat1[!is.na(dat1$gender),]
dat1 = dat1[!is.na(dat1$preferred_offer_types),]

# Split data into numeric and categorical

cn = unlist(lapply(dat1, is.numeric))
dat_n = dat1[, cn]
dat_c = dat1[, setdiff(colnames(dat1), colnames(dat_n))]

# Missing percentage

mean(is.na(dat_n$days_since_last_login))
mean(is.na(dat_n$avg_frequency_login_days))
mean(is.na(dat_n$points_in_wallet))
mean(is.na(dat_c$medium_of_operation))
mean(is.na(dat_c$region_category))

######## NUMERICAL VARIABLES ########

# Fix NAs in Numeric

hist(dat_n$days_since_last_login, main = 'Days since last login', xlab = '', col = 'red')
hist(dat_n$avg_frequency_login_days, main = 'Average frequency login days', xlab = '', col = 'blue')
hist(dat_n$points_in_wallet, main = 'Points in wallet', xlab = '', col = 'green')

imp = mice(dat_n, method = 'pmm', maxit = 20, seed = 5615)
densityplot(imp)
dat_nc = complete(imp, 2)

######## CATEGORICAL VARIABLES ########

apply(select(dat_c, -churn_risk_score), 2, table)

# Bar Charts

lapply(1:14,function(i) {
  ggplot(data.frame(x=select(dat_c, -churn_risk_score)[,i]), aes(x=x, color=x, fill=x))+
    geom_bar()+
    ggtitle(names(select(dat_c, -churn_risk_score))[i])+ 
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    geom_text(stat='count', aes(label=..count..), vjust=2, colour="white")
})

# Merge categorical and numerical data

dat2 = cbind(dat_nc, dat_c)

# Fix NAs in Categorical

dat3 = kNN(dat2, c('medium_of_operation', 'region_category'), weightDist = TRUE )
dat3 = dat3[, 1:21]

######## DIMENSION REDUCTION ########

# Correlation of numerical data

corr = round(cor(dat3[, 1:6]), 2)
melted = melt(cor(na.omit(dat3[, 1:6])))
ggplot(data = melted, aes(x = Var1, y= Var2, fill = value)) + geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white", midpoint = 0, limit = c(-1 ,1))

######## DATA MANIPULATION ########

# Creating dummy variables for categorical

dat.train = dummy_cols(select(dat3, -churn_risk_score),
                       remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE)
dat.train = dat.train %>% cbind(dat3$churn_risk_score) %>%
  rename(churn_risk_score = `dat3$churn_risk_score`)
dat.test = dummy_cols(select(test1, -churn_risk_score),
                      remove_most_frequent_dummy = FALSE, remove_selected_columns = TRUE)
dat.test = dat.test %>% cbind(test1$churn_risk_score) %>% 
  rename(churn_risk_score = `test1$churn_risk_score`)
dat.test = dat.test[, colnames(dat.train)]
names(dat.train) = str_replace_all(names(dat.train), c(' ' = '_', '/' = '_', '-' = '_'))
names(dat.test) = str_replace_all(names(dat.test), c(' ' = '_', '/' = '_', '-' = '_'))

# Standardize numerical variables

dat.train_sc = dat.train
dat.train_sc[, 1:6] = scale(dat.train_sc[, 1:6])
dat.test_sc = dat.test
dat.test_sc[, 1:6] = scale(dat.test_sc[, 1:6])

######## LOGISTIC REGRESSION ########

min.model = glm(dat.train$churn_risk_score ~ 1, data = dat.train, family = 'binomial')
max.model = glm(dat.train$churn_risk_score ~ ., data = dat.train, family = 'binomial')
max.formula = formula(max.model)

#obj_f = step(min.model, direction='forward', scope=max.formula)
obj_b = step(max.model, direction='backward', scope=max.formula)
#obj_fb = step(max.model, direction='both', scope=max.formula)

#summary(obj_f)
summary(obj_b)
#summary(obj_fb)

# get odds ratio for logistic regression

get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}

get.or(summary(obj_b))
# Logistic Regression Prediction 

yhat_lg = predict(obj_b, newdata = dat.test, type='response')
hist(yhat_lg)

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

ypred_lg = dichotomize(yhat_lg)
err_lg = mean(ypred_lg != dat.test$churn_risk_score) # misclassification error rate
err_lg

varImp(obj_b) #Variable importance

######## CART ########

K = 3

fit = rpart(dat.train_sc$churn_risk_score ~ ., method="class", 
            data=dat.train_sc, cp = 1e-2, minsplit=5, xval=K)

# Minimum Error Tree

fit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(fit.me, main = 'Min Error Tree')

# Best Pruned Tree

ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(fit$cptable[1:ind,"xerror"] - xer1) # select tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

# CART Prediction

ypred_cart = predict(pfit.bp, dat.test_sc, type = "class")
err_cart = 1 - mean(ypred_cart == dat.test_sc$churn_risk_score)
err_cart

######## KNN ########

Xtrain = select(dat.train_sc, -churn_risk_score)
ytrain = dat.train_sc$churn_risk_score
Xtest = select(dat.test_sc, -churn_risk_score)
ytest = dat.test_sc$churn_risk_score

get.prob = function(x) {
  prob = attr(x, 'prob')
  cl = as.numeric(x)
  ind = which(cl == 1)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj_knn = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 148, 2), .5)
obj_knn

# KNN Prediction

yhat_knn = knn(Xtrain, Xtest, ytrain, k = obj_knn$k.optimal, prob=T)
prob = get.prob(yhat_knn)
ypred_knn = as.numeric( prob > 0.5 )
err_knn = mean(ypred_knn != as.numeric(ytest) - 1)
err_knn


######## SVM ########

obj_svm = svm(dat.train_sc$churn_risk_score~., data = dat.train_sc, 
              kernel = "polynomial", cost = 7, scale = FALSE, cross = 3)
ypred_svm = predict(obj_svm, dat.test_sc)
err_svm = mean(ypred_svm != dat.test_sc$churn_risk_score)
err_svm

table(ypred_svm)
# SVM Tuning

#svmtune = tune(svm, churn_risk_score~., data = dat.train_sc, 
#               ranges = list(gamma = 2^(-2:2), cost = 5:10),
#               tunecontrol = tune.control(nrepeat = 10, sampling = "cross", cross = 2))

######## RANDOM FOREST ########

yhat_rf = randomForest(churn_risk_score ~ ., data=dat.train_sc, importance=TRUE)
yhat_rf
plot(yhat_rf)
varImpPlot(yhat_rf)

# Random Forest Prediction
ypred_rf <- predict(yhat_rf,dat.test_sc[,-42])
ypred_rf
err_rf = mean(ypred_rf != dat.test_sc$churn_risk_score) # misclassification error rate
err_rf

######## MODEL EVALUATION ########


# ROC all-in-one

par(pty="s") 
lrROC <- roc(dat.test$churn_risk_score ~ ypred_lg,plot=TRUE,print.auc=TRUE,col="red",lwd = 5,legacy.axes=TRUE,main="ROC Curves")

cartROC <- roc(dat.test$churn_risk_score ~ as.numeric(ypred_cart),plot=TRUE,print.auc=TRUE,col="green",lwd = 2,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)

knnROC <- roc(dat.test$churn_risk_score ~ ypred_knn,plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.3,legacy.axes=TRUE,add = TRUE)

svmROC <- roc(dat.test$churn_risk_score ~ as.numeric(ypred_svm),plot=TRUE,print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.2,legacy.axes=TRUE,add = TRUE)

rfROC <- roc(dat.test$churn_risk_score ~ as.numeric(ypred_rf),plot=TRUE,print.auc=TRUE,col="purple",lwd = 4,print.auc.y=0.1,legacy.axes=TRUE,add = TRUE)

legend("bottomright",legend=c("Logistic Regression","CART","KNN","SVM","Random Forest"),col=c("red","green","blue","orange","purple"),lwd=4)


## Confusion matrix
# confusion matrix for logistic regression model

confusionMatrix(as.factor(ypred_lg), dat.test$churn_risk_score)
# confusion matrix for CART
confusionMatrix(as.factor(ypred_cart), dat.test$churn_risk_score)
# confusion matrix for KNN
confusionMatrix(as.factor(ypred_knn), dat.test$churn_risk_score)
# confusion matrix for SVM
confusionMatrix(as.factor(ypred_svm), dat.test$churn_risk_score)
# confusion matrix for Random Forest
confusionMatrix(as.factor(ypred_rf), dat.test$churn_risk_score)  #highest accurary 

## F1 Score
# F1 Score for logistic regression model
F1_Score(dat.test$churn_risk_score, ypred_lg, positive = NULL)
# F1 Score for CART
F1_Score(dat.test$churn_risk_score, ypred_cart, positive = NULL)
# F1 Score for KNN
F1_Score(dat.test$churn_risk_score, ypred_knn, positive = NULL)
# F1 Score for SVM
F1_Score(dat.test$churn_risk_score, ypred_svm, positive = NULL)
# F1 Score for Random Forest
F1_Score(dat.test$churn_risk_score, ypred_rf, positive = NULL) ## highest F score
