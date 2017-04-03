R_squar  <- function(fact,pred) {1-(sum((fact-pred)^2)/
                                      sum((fact-mean(fact))^2))}










table<- as.data.frame(table)
fit_1<- lm(table[,1]~.,table[,-1])
summary(fit_1)


library("MASS")
# library("xgboost")

fit_ridge<-lm.ridge(table[,1]~.,data = table[,-1],lambda = seq(0,400,0.01))
plot(x = fit_ridge$lambda,y = fit_ridge$GCV,type="o")
lambda<-fit_ridge$GCV[which.min(fit_ridge$GCV)]
lamda<-as.numeric(names(lambda))
fit_ridge<-lm.ridge(table[,1]~.,data = table[,-1],lambda = lamda)
beta.fit_ridge<-coef(fit_ridge)
m<-length(coef(fit_ridge))

pred<- beta.fit_ridge[1]+as.matrix(table[,2:ncol(table)])%*%beta.fit_ridge[2:m]


R_squar(table$Цена,pred)


info$price_real <- table$Цена
info$price_lm <- predict(fit_1,newdata = table[,-1])
info$price_ridge <- pred
info$otnoh<- (info$price_ridge+info$price_lm)/2-info$price_real

info<- info[order(otnoh,decreasing = T)]
plot(info$otnoh)


# library("xgboost")
# 
# 
# s<- sample(nrow(table),0.7*nrow(table))
# 
# train<- xgb.DMatrix(as.matrix(table[s,-1]), label=table$Цена[s])
# 
# val<- xgb.DMatrix(as.matrix(table[-s,-1]), label=table$Цена[-s])
# 
# watchlist<-list(val=val,train=train)
# param <- list(  
#                 booster = "gbtree",
#                 eval_metric = 'rmse',
#                 eta                 = 0.2, # 0.06, #0.01,
#                 max_depth           = 6, #changed from default of 8
#                 subsample           = 0.7, # 0.7
#                 colsample_bytree    = 0.77 # 0.7
#                 # num_parallel_tree   = 2
#                 # alpha = 0.0001,
#                 # lambda = 1
#                 # lambda_bias = 1
# )
# 
# clf2<- xgb.train(data = train, nrounds = 10000, params = param,
#           watchlist =watchlist)
# 
# 
# 
# 
# 
# which.min(clf2$evaluation_log$val_rmse)
# pred<- predict(clf2,as.matrix(table[,-1]),ntreelimit = which.min(clf2$evaluation_log$val_rmse))


info
fwrite(info,"order_2.csv")


info <- info[price_real>35000]
