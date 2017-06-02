
comp = function(x,y){
  a = unique(x)[!(unique(x) %in% y)]
  b = unique(y)[!(unique(y) %in% x)]
  cat("uniqueA: ")
  for (i in a){
      cat(i,sum(x==i),", ")
  }
  cat ('\n')
  cat("uniqueB: ")
  for (j in b){
      cat(j,sum(y==j),", ")
  }
  cat ('\n')
}

env = new.env()
load("dataN11.RData",env )
data.N11 = env$data.N11

env = new.env()
load("dataN14.RData",env )
data.N14 = env$data.N14

dim(data.N11)
dim(data.N14)

# data.N11 only has one more column: connect
sum(names(data.N11)==names(data.N14))

# Only variable with true missing NAs
table(data.N11$headAge,useNA = "always")

# There should be no NAs except for headAge and secAge (age of village head and secretary)
# We delete these two variables as retaining the rows is more important than retaining these columns
data.N11=data.N11[,!names(data.N11) %in% c('headAge','secAge')]
dim(data.N11)

N11.ID = data.N11[,'villageID']
N11.data = data.N11[,-1]

dim(data.N14)
N14.ID = data.N14[,'villageID']
N14.data = data.N14[,-1]

library(glmnet)

x.11 = sparse.model.matrix(connect~.,N11.data)[,-1] 
y.11 = N11.data$connect

dim(x.11)
table(y.11)

lasso.11 = cv.glmnet(x.11,y.11,alpha = 1, type.measure = "class", family = "binomial")

plot(lasso.11)
print (lasso.11)

library(AUC)
pred.11 = predict(lasso.11, x.11, type='response')
auc(roc(pred.11, factor(y.11)))
plot(roc(pred.11, factor(y.11)), main = "ROC",col="red")

coef.N11 = predict(lasso.11, newx = x.11, s = "lambda.1se", type = "coefficients")
coef.N11 = as.matrix(coef.N11)
coef.N11 = coef.N11[coef.N11!=0,,drop=FALSE]

write.csv(coef.N11,"coefN11.csv")

coef.N11
length(coef.N11)

sapply(data.N14,function(x){mean(is.na(x)==TRUE)})
data.N14=data.N14[,!names(data.N14) %in% c('headAge','secAge')]

x.14 = sparse.model.matrix(~.,N14.data)[,-1]
dim(x.14)

N14.pred = predict(lasso.11, newx = x.14, s = "lambda.1se", type = "response")
N14.pred = data.frame(N14.ID,N14.pred)

write.csv(N14.pred,"Prediction_2014.csv",row.names=FALSE)

x.11NN = model.matrix(connect~., N11.data)[,-1]
y.11 = N11.data$connect
dim(x.11NN)

head(x.11A)

library(glmnet)
library(h2o)
localH2O =  h2o.init(nthreads = -1, port = 54321, max_mem_size = '6G', startH2O = TRUE)

NN.train = as.h2o(x.11NN)
NN.train

# One layer Autoencoder

par(mfrow = c(2,2))
for (i in seq(100,600,100)){
    
    NN.model <- h2o.deeplearning(    
      x = 1:ncol(NN.train), 
      training_frame = NN.train,   
      hidden = c(i), # number of layers and their units
      epochs = 50, # maximum number of epoches  
      activation = 'Tanh', # activation function 
      autoencoder = TRUE, 
      l2 = 0.1# learning rate
    )
    
    NN.newFeature = h2o.deepfeatures(NN.model, NN.train,layer = 1)
    NN.newFeature = as.data.frame(NN.newFeature)
    NN.newFeature = sparse.model.matrix(~.,NN.newFeature)[,-1]
    dim(NN.newFeature)
    lasso2.l1 = cv.glmnet(NN.newFeature,y.11, alpha = 1, family = 'binomial',type.measure ='class')
    plot(lasso2.l1,main = i)
    
}

