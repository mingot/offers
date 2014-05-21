

setwd("/Users/mingot/Projectes/kaggle/Offers")
train = read.table(file="data/original/trainHistory.csv", header=T, sep=",")
test = read.table(file="data/original/testHistory.csv", header=T, sep=",")
offers = read.table(file="data/original/offers.csv", header=T, sep=",")

train$repeater = as.numeric(train$repeattrips>0)

# Extract Features --------------------------------------------------------
library(data.table)
library(bit64)

# Create data tables (~10 min)
TRAIN = data.table(merge(train, offers[,c("offer","company","brand","category")], all.x=T))
TRANS = fread("data/sample/transactions_train.csv", header=T)
TEST = data.table(merge(test, offers[,c("offer","company","brand","category")], all.x=T))
TRANS_TEST = fread("data/sample/transactions_test.csv")

setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))
setnames(TRANS_TEST, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))

# # Offers priors
# offPrior = setnames(aggregate(train$repeater, by=list(train$offer), FUN=mean), c("offer","offer_prior")) 
# offPrior = offPrior[offPrior$offer %in% unique(test$offer),]
# 
# # Company priors
# compPrior = setnames(aggregate(t$repeater, by=list(t$company), FUN=mean), c("company","company_prior"))
# compPrior = compPrior[compPrior$company %in% unique(tTest$company),]


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
  S = TRANS[,list(product_users=.N), by=list(id,category,brand,company)]
  S = S[,list(product_users=.N), by=list(category,brand,company)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  # product price
  S = TRANS[purchaseamount>0 & purchasequantity>0,c("purchaseamount","purchasequantity","brand","company","category"), with=F] # remove negative transactions
  S = S[,prize:=purchaseamount/purchasequantity]
  S = S[,list(prize=mean(prize)), by=list(brand,company,category)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  # data treatment
  t = data.frame(TRAIN)
  
  # check for brands
  t[is.na(t)] = 0 # NA treatment
  t$check_brand = as.numeric(t$BRANDquant!=0)
  t$check_category = as.numeric(t$CATquant!=0)
  t$check_company = as.numeric(t$COMPquant!=0)
  
  # Scaling
  # t[,11:16] = scale(t[,11:16])
  t$aov_sc = scale(t$aov)
  t$freq_sc = scale(t$freq)
  t$product_times_sc = scale(t$product_times)
  t$product_users_sc = scale(t$product_users)
  
  # priors to known offers
  # t = merge(t, offPrior, all.x=T) 
  
  # NA treatment
  t[is.na(t)] = 0

  return(t)
}

l = c(64,152,166)
var = "chain"
for(v in l)
  t[,paste(var,v,sep="")] = as.numeric(t[,var]==v)

l = c(1,15,21,96)
var = "market"
for(v in l)
  t[,paste(var,v,sep="")] = as.numeric(t[,var]==v)



# ~10min
t = ExtractFeatures(TRANS, TRAIN)


# training ----------------------------------------------------------------
library(pROC)
library(gbm)


k = 3 # Number of k-folds
id = sample(1:k,nrow(t),replace=TRUE)
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = t[id %in% list[-i],]
  testset = t[id %in% c(i),]
  
  # Training
  trainingset = trainingset[,!names(trainingset) %in% c("repeattrips","offerdate")]

#   fit.gbm = gbm(repeater ~ . -offer -id -aov -freq -product_times, 
#                 data=trainingset, distribution="adaboost")

  fit.glm = glm(repeater ~ check_company + check_category + check_brand 
                + check_brand*check_category*check_company
                + aov_sc + freq_sc + product_times_sc  + product_users_sc
                + chain64 + chain152 + chain166
                #+ factor(market),
                + market1 + market15 + market21 + market96, 
                data=trainingset, family=binomial)
  
  # Testing
  pred = predict(fit.glm, testset, type="response")
  real = testset$repeater
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")
summary(fit.glm)

# Auxiliar
fit.glm = glm(repeater ~ check_company + check_category + check_brand + check_combined
              + aov_sc + freq_sc + product_times_sc  + product_users_sc
              + chain64 + chain152 + chain166
              + factor(market),
              data=t, family=binomial)

write(names(fit.glm$coefficients[1:16]), file="") # list all the variables used
cat(names(fit.glm$coefficients[2:13]),sep=", ") # list all the variables used (1 line)


# step selection
library(MASS)
step = stepAIC(fit.glm, direction="both")
summary(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")])
summary(scale(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")]))

ids_trans = c(TRANS[["id"]], TRANS_TEST[["id"]])
ids_trans = unique(ids_trans)


# Prediction --------------------------------------------------------------

# (~ 10min)
tTest = ExtractFeatures(TRANS_TEST, TEST)
pred = predict(fit.glm, tTest, type="response")
summary(pred)

# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=tTest$id, repeatProbability=pred), file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")

