
library(doSNOW)
library(doParallel)
library(parallel)

library(xgboost)
nfold = 5
fcrossv = function(param,nfold,vb=1){
  set.seed(1235)
  xgb.cv(param,
         dtrain,nrounds = 100000,
         nfold = nfold,
         maximize = F,
         early_stopping_rounds = 100,
         print_every_n = 200,
         verbose = 1)
}

# vimp = colnames(df_train)
# crossv = fcrossv(param,nfold)
# nrounds = crossv$best_ntreelimit
# 
# set.seed(1235)
# mod0 = xgb.train(data = dtrain,
#                  params = param,
#                  nrounds = nrounds,
#                  maximize =  F,
#                  verbose = 1)
# 
# vmodel = colnames(dtrain)
# imp = as.data.frame(xgb.importance(feature_names = vmodel,model = mod0))
# imp$cumGain = cumsum(imp$Gain)
# imp$n = 1:nrow(imp)
# impgap = imp
# gc()

start.time <- Sys.time()
# Create a cluster to work on 10 logical cores.
cl <- makeCluster(10, type = "PSOCK")
registerDoParallel(cl)

s= Sys.time()
nfeat= length(impgap$Feature)
eval = 1.2
best = -1
eps = 0
featbag = impgap$Feature[1]
for (idx in 2:nfeat) {
  vadd = imp$Feature[idx]
  vimp = c(featbag,vadd)
  dtrain = xgb.DMatrix(as.matrix(df_train[,vimp]), label = label2)
  
  crossv = fcrossv(param,nfold,1)
  nrnds = crossv$best_ntreelimit
  
  tmpeval = crossv$evaluation_log$test_mlogloss_mean[nrnds]
  if((tmpeval+eps)<eval){eval = tmpeval; best = idx; featbag=vimp}
  
  print(c(idx,eval,best,length(featbag)));flush.console()
  rm(dtrain,crossv)
  gc()
}

Sys.time() 



stopCluster(cl)
Sys.time() -  start.time
