set.seed(123)
# Classification Tree
require(rpart)
#require(tree)
require(aplore3)
data(icu)
# Split the data into two parts, training (80%) and testing (20%). 
# A cross-validation will be performed on the training part later.
train=sample.int(nrow(icu),floor(nrow(icu)*0.8))
test=(1:nrow(icu))[-train]
# fit a tree on the dataset icu
fit=rpart(sta~gender+age+race+loc, data=icu, subset=train)
par(mfrow = c(1,2), xpd = NA)
plot(fit)
text(fit, pretty=0)
plot(fit)
text(fit, pretty=0, use.n = TRUE)
dev.off()
printcp(fit)
# nsplit = size of tree - 1
# the root node error is error of a simple majority voting 
# rel_error is the relative training error, comparing with the root node error
# xerror is the relative validation error, comparing with the root node error
plotcp(fit)
# There are usally two approaches to select the optimal cp (or the size of tree)
# 1. Select the cp which provides the smallest xerror, if there are multiple cp's providing
# the smallest xerror, choose the largest one among them.
# 2. Select the largest cp such that xerror<min(error)+xstd, this is suggested in help(plotcp).

# approach 1
cp1=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
fit_prune1=prune(fit, cp=cp1)
par(mfrow = c(1,2), xpd = NA)
plot(fit_prune1)
text(fit_prune1, pretty=0)
plot(fit_prune1)
text(fit_prune1, pretty=0, use.n = TRUE)

# approach 2

cp2=fit$cptable[min(which(fit$cptable[,"xerror"]<=min(fit$cptable[,"xerror"])+fit$cptable[,"xstd"])),"CP"]
fit_prune2=prune(fit, cp=cp2)
par(mfrow = c(1,2), xpd = NA)
plot(fit_prune2)
text(fit_prune2, pretty=0)
plot(fit_prune2)
text(fit_prune2, pretty=0, use.n = TRUE)
fit_prune2 

# Let's obtain the testing error of the two approaches
predict_table1=table(predict(fit_prune1,newdata=icu[test,],type="class"),icu$sta[test])
err_test1=1-sum(diag(predict_table1))/sum(predict_table1) #the testing error 
predict_table2=table(predict(fit_prune2,newdata=icu[test,],type="class"),icu$sta[test])
err_test2=1-sum(diag(predict_table2))/sum(predict_table2) #the testing error 
err_test_guess=min(prop.table(table(icu$sta[test]))) #the testing error of a majority voting
err_test1/err_test_guess #the relative testing error
err_test2/err_test_guess

# Repeat the training-testing split for 10 times
B=10
err_test1=numeric()
err_test2=numeric()
err_test_guess=numeric()
for (b in 1:B){
  train=sample.int(nrow(icu),floor(nrow(icu)*0.8))
  test=(1:nrow(icu))[-train]
  fit=rpart(sta~gender+age+race+loc, data=icu, subset=train)
  cp1=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
  fit_prune1=prune(fit, cp=cp1)
  predict_table1=table(predict(fit_prune1,newdata=icu[test,],type="class"),icu$sta[test])
  err_test1[b]=1-sum(diag(predict_table1))/sum(predict_table1) #the testing error 
  cp2=fit$cptable[min(which(fit$cptable[,"xerror"]<=min(fit$cptable[,"xerror"])+fit$cptable[,"xstd"])),"CP"]
  fit_prune2=prune(fit, cp=cp2)
  predict_table2=table(predict(fit_prune2,newdata=icu[test,],type="class"),icu$sta[test])
  err_test2[b]=1-sum(diag(predict_table2))/sum(predict_table2) #the testing error 
  err_test_guess[b]=min(prop.table(table(icu$sta[test]))) #the testing error of a majority voting
}
mean(err_test1)/mean(err_test_guess) #the overall relative testing error
mean(err_test2)/mean(err_test_guess) 




# Random Forest
require(randomForest)

fit=randomForest(sta~gender+age+race+loc, data=icu, importance=TRUE) #number of trees and m are chosen by default
fit$err.rate[fit$ntree,1] #OOB error
fit$importance #variable importance

rf_oob=function(x){ #the oob error of randomForest using a pair of (ntree, mtry)
  ntree=x[1]
  mtry=x[2]
  fit=randomForest(sta~gender+age+race+loc, data=icu, importance=TRUE,
                   ntree=ntree, mtry=mtry, subset=train)
  fit$err.rate[fit$ntree,1]
}

train=sample.int(nrow(icu),floor(nrow(icu)*0.8)) #split the dataset
test=(1:nrow(icu))[-train]
grid=expand.grid(ntree=50*(1:10), mtry=1:4) #perform RF on a grid of parameters
obb_err=apply(grid,1,rf_oob)
par=grid[which.min(obb_err),] #the optimal parameters which provide the smallest oob error
fit=randomForest(sta~gender+age+race+loc, data=icu, importance=TRUE,
                 ntree=par$ntree,mtry=par$mtry,subset=train)
pred=predict(fit,newdata=icu[test,],type="class")
predict_table=table(pred,icu$sta[test])
err_test=1-sum(diag(predict_table))/sum(predict_table) #the testing error 

# Repeat the training-testing split for 10 times
B=10
err_test3=numeric()
err_test_guess3=numeric()
for (b in 1:B){
  train=sample.int(nrow(icu),floor(nrow(icu)*0.8))
  test=(1:nrow(icu))[-train]
  grid=expand.grid(ntree=50*(1:10), mtry=1:4)
  obb_err=apply(grid,1,rf_oob)
  par=grid[which.min(obb_err),] #the optimal parameters which provide the smallest oob error
  fit=randomForest(sta~gender+age+race+loc, data=icu, importance=TRUE,
                   ntree=par$ntree,mtry=par$mtry,subset=train)
  pred=predict(fit,newdata=icu[test,],type="class")
  predict_table=table(pred,icu$sta[test])
  err_test3[b]=1-sum(diag(predict_table))/sum(predict_table) #the testing error 
  err_test_guess3[b]=min(prop.table(table(icu$sta[test]))) #the testing error of a majority voting
}
mean(err_test3)/mean(err_test_guess3) #the overall relative testing error

# To compare the testing error of tree and random forest, it would be better to
# use the same training-testing split, by putting them in the same for loop.






