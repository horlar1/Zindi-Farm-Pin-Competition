
library(xgboost)
# o = c(4)
label2 = label -1
# score = c()
# p = matrix(ncol = 9,nrow = 1074)
# for (i in o) {
#   
# print(paste0("model training on label ", i))
#   
#   label = cv_label[,i]

dtrain = xgb.DMatrix(as.matrix(df_train), label=label2)
dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))


param = list(booster = "gbtree",
             objective = "multi:softprob",
             eval_metric = "mlogloss",
             eta = 0.01,
             colsample_bytree = 0.5,
             max_depth = 4,
             min_child_weight = 1,
            # num_parallel_tree = 10,
             nthread = 8,
          
             gamma = 0,
             subsample = 0.8,
             num_class = 9
)

watchlist= list(train = dtrain)

set.seed(1235)
fit_cv = xgb.cv(params = param,
                data = dtrain,
                watchlist = watchlist,
                nrounds = 10000,
                nfold = 10,
                print_every_n = 200,
                early_stopping_rounds = 100,
                prediction = TRUE,
                maximize = FALSE)

rnd = fit_cv$best_ntreelimit

#score = fit_cv$evaluation_log$test_logloss_mean[rnd]

# set.seed(1235)
# mod.xgb = xgb.train(data = dtrain,params = param,nrounds = rnd)
# 
# p[,i] = predict(mod.xgb,dtest)

#}


set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = param,nrounds = 1352)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = mod.xgb))


pred= predict(mod.xgb,dtest)

pred = matrix(pred,nrow = 9,ncol = length(pred)/9) %>% t()
sub = cbind(test.id,pred)
colnames(sub) = c("field_id",paste0("crop_id",1:9))
write.csv(sub,file = "sub_4.csv",row.names = F)


#########


### Featire selection process
nfold = 5
fcrossv = function(param,nfold,vb=1){
  set.seed(1235)
  xgb.cv(param,
         dtrain,nrounds = 100000,
         nfold = nfold,
         maximize = F,
         early_stopping_rounds = 50,
         print_every_n = 200,
         verbose = 1)
}

vimp = colnames(df_train)
crossv = fcrossv(param,nfold)
nrounds = crossv$best_ntreelimit

set.seed(1235)
mod0 = xgb.train(data = dtrain,
                 params = param,
                 nrounds = nrounds,
                 maximize =  F,
                 verbose = 1)

vmodel = colnames(dtrain)
imp = as.data.frame(xgb.importance(feature_names = vmodel,model = mod0))
imp$cumGain = cumsum(imp$Gain)
imp$n = 1:nrow(imp)
impgap = imp
gc()

s= Sys.time()
nfeat= length(impgap$Feature)
eval = 0.68210
best = -1
eps = 0
featbag = impgap$Feature[1:40]
for (idx in 41:nfeat) {
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

Sys.time() -s






### log loss trick
a = pred
for (i in 1:nrow(a)) {
  j = 1:9
  row = a[i,]
  max_val = max(row)
  preds = match(max_val, row)
  if(max_val > 0.9){
    a[i,preds] = 1
    a[i,j!= preds] = 0
  }
}

a = pred

for (i in 1:nrow(a)) {
  j = 1:9
  preds = 1
  if(a[i,preds] > 0.6){
    a[i,preds] = 1
    a[i,j != preds]=0
  }
}


library(h2o)

h2o.init(nthreads = 6)
tr = cbind(df_train,label2=as.factor(label2))

htrain = as.h2o(tr)
dtest = as.h2o(df_test)

aml = h2o.automl(x = seq(1,ncol(htrain)),
                 y = ncol(htrain),
                 training_frame = htrain,
                 project_name = "auto1",
                 nfolds = 5,seed = 1235,
                 max_runtime_secs = 600,
                 stopping_metric = "logloss",
                 exclude_algos = "GLM"
                 )
####
p = as.data.frame(h2o.predict(aml@leader,newdata = dtest))
pred = as.data.frame(h2o.predict(aml,newdata = dtest))











#########
library(xgboost)
# b = 0:7
# #label2 = label -1
# score = c()
# #p = rep(0, nrow(df_test))
# for (i in b) {
#   
#   print(paste0("model training on label ", i))
#   
#   # label2 = as.data.frame(cv_label) %>% filter(country == i) %>% 
#   #   select(label) 
#   d = df_train %>% filter(sub == i)
#   label = d$label
#   d$label = NULL
#   d$sub = NULL
#  # d2 = df_test %>% filter(country == i)
#   label_enc= LabelEncoder.fit(as.factor(label))
#   label2 = transform(label_enc,as.factor(label))
#   label2 = label2 -1 
#   
#   
#   dtrain = xgb.DMatrix(as.matrix(d), label=label2)
#   #dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))
#   
#   lm = length(unique(label))
#   
#   param = list(booster = "gbtree",
#                objective = "multi:softprob",
#                eval_metric = "mlogloss",
#                eta = 0.01,
#                colsample_bytree = 0.5,
#                max_depth = 4,
#                min_child_weight = 1,
#                # num_parallel_tree = 10,
#                nthread = 8,
#                
#                gamma = 0,
#                subsample = 0.8,
#                num_class = lm
#   )
#   
#   watchlist= list(train = dtrain)
#   
#   set.seed(1235)
#   fit_cv = xgb.cv(params = param,
#                   data = dtrain,
#                   watchlist = watchlist,
#                   nrounds = 10000,
#                   nfold = 10,
#                   print_every_n = 200,
#                   early_stopping_rounds = 100,
#                   prediction = TRUE,
#                   maximize = FALSE)
#   
#   rnd = fit_cv$best_ntreelimit
#   
#   score = fit_cv$evaluation_log$test_logloss_mean[rnd]
# 
#   # set.seed(1235)
#   # mod.xgb = xgb.train(data = dtrain,params = param,nrounds = rnd)
#   # 
#   # p[,i] = predict(mod.xgb,dtest)
# 
# }



library(CatEncoders)
label_enc= LabelEncoder.fit(as.factor(label))
label2 = transform(label_enc,as.factor(label))

