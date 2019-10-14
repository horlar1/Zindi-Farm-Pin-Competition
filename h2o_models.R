
library(h2o)

h2o.init(nthreads = -1)
tr = cbind(df_train,label=as.factor(label2))

htrain = as.h2o(tr)
dtest = as.h2o(df_test[,colnames(df_train)])

dl_model = h2o.deeplearning(x = seq(1,ncol(htrain)),
                      y = ncol(htrain),
                      training_frame = htrain,
                      model_id = "dl1",
                      nfolds = 5,seed = 1235,
                      fold_assignment = "Stratified",
                      activation = "RectifierWithDropout",
                      #shuffle_training_data = TRUE,
                    #  distribution = "bernoulli",
                      standardize = T,
                      train_samples_per_iteration = -1,
                      hidden = c(200,200),#,200,200),
                      l1 = 0.000001,
                     mini_batch_size = 100,
                     #l2 = 2,
                      max_w2 = 10,
                     # balance_classes = TRUE,
                     # loss = "CrossEntropy",
                      hidden_dropout_ratios = c(0.4,0.4),#0.2,0.2),
                     reproducible = T,
                      epochs = 30,
                      variable_importances = TRUE,
                      stopping_metric = "logloss",
                      rate = 0.01,
                      stopping_rounds = 100,
                      verbose = FALSE)


p = as.data.frame(h2o.predict(dl_model,newdata = dtest))
pred = as.data.frame(h2o.predict(aml,newdata = dtest))





start.time = Sys.time()
set.seed(1235)
model = train(x = df_train, y = as.factor(label2), method ="gbm",
              metric = "logLoss",
              trControl = trainControl(
                method = "cv",
                number = 10,
                verboseIter = T,
              classProbs = T,
              summaryFunction =multiClassSummary,
                savePredictions = "final"
              ))

total.time = Sys.time() - start.time
total.time 