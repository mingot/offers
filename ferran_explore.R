

train = read.table(file="data/original/trainHistory.csv", header=T, sep=",")
train$repeater = 0
train$repeater[train$repeattrips > 0] = 1
#test = read.table(file="data/original/testHistory.csv", header=T, sep=",")
offers = read.table(file="data/original/offers.csv", header=T, sep=",")


# Extract Features --------------------------------------------------------
library(data.table)
library(bit64)

# Create data tables
TRAIN = data.table(merge(train, offers[,c("offer","company","brand","category")], all.x=T))
TRANS = fread("data/awk/transactions_sample.csv", header=T)
#TEST = data.table(merge(test, offers[,c("offer","company","brand","category")], all.x=T))
#TRANS_TEST = fread("data/sample/transactions_test.csv")

#setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))

# Filter users 
set(TRANS, j="id", value=as.numeric(TRANS[["id"]]))
set(TRANS, j="company", value=as.numeric(TRANS[["company"]]))
TRAIN = TRAIN[which(TRAIN$id%in%TRANS$id),]

# Offers priors
# train$repeater = 0
# train$repeater[train$repeattrips>0] = 1
# offPrior = aggregate(train$repeater, by=list(train$offer), FUN=mean)
# names(offPrior) = c("offer","offer_prior")
# offPrior = offPrior[offPrior$offer %in% unique(test$offer),]


ExtractFeatures = function(TRANS, TRAIN){
  # convert from int64 to numeric some fields
  set(TRANS, j="id", value=as.numeric(TRANS[["id"]]))
  set(TRANS, j="company", value=as.numeric(TRANS[["company"]]))
  
  # brand grouping
  setkey(TRANS,id,brand)
  setkey(TRAIN,id,brand)
  S = TRANS[,list(BRANDquant=sum(purchasequantity), BRANDamount=sum(purchaseamount)), by=list(id,brand)]
  TRAIN = merge(TRAIN, S, all.x=T)
  
  # company grouping
  setkey(TRANS,id,company)
  setkey(TRAIN,id,company)
  S = TRANS[,list(COMPquant=sum(purchasequantity), COMPamount=sum(purchaseamount)), by=list(id,company)]
  TRAIN = merge(TRAIN, S, all.x=T)
  
  # category grouping
  setkey(TRANS,id,category)
  setkey(TRAIN,id,category)
  S = TRANS[,list(CATquant=sum(purchasequantity), CATamount=sum(purchaseamount)), by=list(id,category)]
  TRAIN = merge(TRAIN, S, all.x=T) 
  
  # AOV
  S = TRANS[,list(aov=sum(purchaseamount)), by=list(id, date)]
  S = S[,list(aov=mean(aov)), by=id]
  TRAIN = merge(TRAIN, S, by="id", all.x=T)
  
  # frequency (times per month)
  S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date)] #group by dayly transactions
  S = S[, month:=substr(S[["date"]],1,7)] # extract month-year
  S = S[, times:=.N, by=list(id,month)] # group by month
  S = S[, list(freq=mean(times)), by=id] # mean per user
  TRAIN = merge(TRAIN, S, by="id", all.x=T)
  
  # number of times the offered product was bought
  S = TRANS[,list(product_times=.N), by=list(category,brand,company)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  # number of different users that boutht the product
  
  # data treatment
  t = data.frame(TRAIN)
  
  # check for brands
  t[is.na(t)] = 0 # NA treatment
  t$check_brand = 0
  t[t$BRANDquant!=0,"check_brand"] = 1
  t$check_category = 0
  t[t$CATquant!=0,"check_category"] = 1
  t$check_company = 0
  t[t$COMPquant!=0,"check_company"] = 1
  t$check_combined = t$check_brand*t$check_category*t$check_company
  
  # Scaling
  # t[,11:16] = scale(t[,11:16])
  t$aov_sc = scale(t$aov)
  t$freq_sc = scale(t$freq)
  t$product_times_sc = scale(t$product_times)
  
  # priors to known offers
  # t = merge(t, offPrior, all.x=T) 
  
  # NA treatment
  t[is.na(t)] = 0
  
  return(t)
}

t = ExtractFeatures(TRANS, TRAIN)

# Exploration
t$company = as.factor(t$company)
aux1 = aggregate(repeater~company, data = t, FUN = sum)
t$ones = 1
aux2 = aggregate(ones~company, data = t, FUN = sum)
exploration1 = merge(aux1,aux2,by = "company")
exploration1$prob = exploration1$repeater/exploration1$ones

t$category = as.factor(t$category)
dataVisualization("repeater","category",t,"categorical")

t$offer = as.factor(t$offer)
dataVisualization("repeater","offer",t,"categorical")

t$chain = as.factor(t$chain)
dataVisualization("repeater","chain",t,"categorical")
aux1 = aggregate(repeater~chain,data = t, FUN = sum)
aux2 = aggregate(ones~chain, data = t, FUN = sum)
exploration1 = merge(aux1,aux2,by = "chain")
exploration1$prob = exploration1$repeater/exploration1$ones
exploration1 = exploration1[order(-exploration1$ones,-exploration1$prob),]

t$market = as.factor(t$market)
dataVisualization("repeater","market",t,"categorical")
aux1 = aggregate(repeater~market,data = t, FUN = sum)
aux2 = aggregate(ones~market, data = t, FUN = sum)
exploration1 = merge(aux1,aux2,by = "market")
exploration1$prob = exploration1$repeater/exploration1$ones
exploration1 = exploration1[order(-exploration1$ones,-exploration1$prob),]

# training ----------------------------------------------------------------
library(pROC)
library(gbm)

k = 5 # Number of k-folds
id = sample(1:k,nrow(t),replace=TRUE)
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = t[id %in% list[-i],]
  testset = t[id %in% c(i),]
  
  # Training
  trainingset = trainingset[,!names(trainingset) %in% c("repeattrips","offerdate")]
  #   fit.glm = glm(repeater ~ . -offer -id -aov -freq -product_times
  #                  + factor(market) - chain
  #                 - brand - company - category - market, 
  #                 data=trainingset, family=binomial)
  #   fit.gbm = gbm(repeater ~ . -offer -id -aov -freq -product_times, 
  #                 data=trainingset, distribution="adaboost")
  #   fit.glm = glm(repeater ~ BRANDamount + CATquant + COMPquant + COMPamount, data=trainingset, family=binomial)
  fit.glm = glm(repeater ~ check_company + check_category + check_brand + aov_sc + freq_sc 
                + product_times_sc + check_combined + factor(market), 
                data=trainingset, family=binomial)
  #   fit.gbm = gbm(repeater ~ check_company + check_category + check_brand + aov_sc + freq_sc + product_times_sc + check_combined,
  #                 distribution="adaboost", data=trainingset) 
  
  # Testing
  pred = predict(fit.glm, testset, type="response")
  real = testset$repeater
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")

# Auxiliar
fit.glm = glm(repeater ~ check_company + check_category + check_brand, data=t, family=binomial) # train the complete model
write(names(fit.glm$coefficients[1:16]), file="") # list all the variables used
cat(names(fit.glm$coefficients[2:9]),sep=", ") # list all the variables used (1 line)


# step selection
library(MASS)
step = stepAIC(fit.glm, direction="both")
summary(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")])
summary(scale(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")]))


# Prediction --------------------------------------------------------------

tTest = ExtractFeatures(TRANS_TEST, TEST)
pred = predict(fit.glm, tTest, type="response")
summary(pred)

# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=tTest$id, repeatProbability=pred), file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")
