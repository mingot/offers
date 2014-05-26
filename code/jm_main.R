
setwd("/Users/mingot/Projectes/kaggle/Offers")
train = read.table(file="data/original/trainHistory.csv", header=T, sep=",")
test = read.table(file="data/original/testHistory.csv", header=T, sep=",")
offers = read.table(file="data/original/offers.csv", header=T, sep=",")

train$repeater = as.numeric(train$repeattrips>0)

# Extract Features --------------------------------------------------------
library(data.table)
library(bit64)

# Create data tables (~10 min)
TRAIN = data.table(merge(train, offers[,c("offer","company","brand","category","offervalue")], all.x=T))
TRANS = fread("data/sample/transactions_train.csv", header=T)
setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))


# # Offers priors
# offPrior = setnames(aggregate(train$repeater, by=list(train$offer), FUN=mean), c("offer","offer_prior"))
# offPrior = offPrior[offPrior$offer %in% unique(test$offer),]
#
# # Company priors
# compPrior = setnames(aggregate(t$repeater, by=list(t$company), FUN=mean), c("company","company_prior"))
# compPrior = compPrior[compPrior$company %in% unique(tTest$company),]

# freq_category and freq_product 

FreqVars = function(TRANS, TRAIN, name){
  # Returns times/month the user goes shopping variable "name" (brand, company, category)
  #     name="category"
  #     name="product"
  S = TRANS[,list(value=sum(purchaseamount)), by=c('id',name,'date')]
  S$value[S$value!=0]=1
  S = S[, month:=substr(S[["date"]],1,7)] # extract month-year
  S = S[, times:=sum(value), by=c("id","month",name)] # group by month
  S = S[, list(aux=mean(times)), by=c("id",name)] # mean per user
  S[is.na(S)] = 0
  
  setnames(S,c("id",name, paste("freq", name, sep = "_")))
  
  TRAIN = merge(TRAIN, S, by = c("id",name),all.x=T) 
  TRAIN[is.na(TRAIN)] = 0
  return(TRAIN)
}



ExtractFeatures = function(TRANS, TRAIN){
  # convert from int64 to numeric some fields
  set(TRANS, j="id", value=as.numeric(TRANS[["id"]]))
  set(TRANS, j="company", value=as.numeric(TRANS[["company"]]))
  
  # brand grouping
  cat("Brand grouping...\n")
  setkey(TRANS,id,brand)
  setkey(TRAIN,id,brand)
  S = TRANS[,list(BRANDquant=sum(purchasequantity), BRANDamount=sum(purchaseamount)), by=list(id,brand)]
  TRAIN = merge(TRAIN, S, all.x=T)
  
  # company grouping
  cat("company grouping...\n")
  setkey(TRANS,id,company)
  setkey(TRAIN,id,company)
  S = TRANS[,list(COMPquant=sum(purchasequantity), COMPamount=sum(purchaseamount)), by=list(id,company)]
  TRAIN = merge(TRAIN, S, all.x=T)
  
  # category grouping
  cat("Category grouping...\n")
  setkey(TRANS,id,category)
  setkey(TRAIN,id,category)
  S = TRANS[,list(CATquant=sum(purchasequantity), CATamount=sum(purchaseamount)), by=list(id,category)]
  TRAIN = merge(TRAIN, S, all.x=T)
  
  # AOV
  cat("AOV...\n")
  S = TRANS[,list(aov=sum(purchaseamount)), by=list(id, date)]
  S = S[,list(aov=mean(aov)), by=id]
  TRAIN = merge(TRAIN, S, by="id", all.x=T)
  TRAIN$aov[TRAIN$aov > 300] = 300
  TRAIN$aov_factor= cut(TRAIN$aov,breaks = c("-100","30","60","1000"), labels = c("low","medium","high"))
  
  # frequency (times per month)
  cat("Frequency...\n")
  S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date)] #group by dayly transactions
  S = S[, month:=substr(S[["date"]],1,7)] # extract month-year
  S = S[, times:=.N, by=list(id,month)] # group by month
  S = S[, list(freq=mean(times)), by=id] # mean per user
  TRAIN = merge(TRAIN, S, by="id", all.x=T)
  
  # number of times the offered product was bought
  cat("Number of times...\n")
  S = TRANS[,list(product_times=.N), by=list(category,brand,company)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  # number of different users that bought the product
  cat("Number of users...\n")
  S = TRANS[,list(product_users=.N), by=list(id,category,brand,company)]
  S = S[,list(product_users=.N), by=list(category,brand,company)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  # product price
  cat("Prize...\n")
  S = TRANS[purchaseamount>0 & purchasequantity>0,c("purchaseamount","purchasequantity","brand","company","category"), with=F] # remove negative transactions
  S = S[,prize:=purchaseamount/purchasequantity]
  S = S[,list(prize=mean(prize)), by=list(brand,company,category)]
  S = S[,category_prize:=mean(prize), by=list(category)]
  TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)
  
  cat("Freq category...\n")
  TRAIN = FreqVars(TRANS,TRAIN,"category")
  cat("Freq product...\n")
  TRAIN$product = paste(TRAIN$brand,TRAIN$category,TRAIN$company,sep = "_")
  TRANS$product = paste(TRANS$brand,TRANS$category,TRANS$company,sep = "_")
  TRAIN = FreqVars(TRANS,TRAIN,"product")
  
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
  t$prize_category_sc = t$prize/t$category_prize
  
  # priors to known offers
  # t = merge(t, offPrior, all.x=T)
  
  l = c(64,152,166)
  var = "chain"
  for(v in l)
    t[,paste(var,v,sep="")] = as.numeric(t[,var]==v)
  
  l = c(1,15,21,96)
  var = "market"
  for(v in l)
    t[,paste(var,v,sep="")] = as.numeric(t[,var]==v)
  
  # discount
  t$discount = t$offervalue/t$prize
  
  # NA treatment
  t[is.na(t)] = 0
  
  return(t)
}

# ~10min
ptm = proc.time()
t = ExtractFeatures(TRANS, TRAIN)
proc.time() - ptm

rm(TRANS)

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
  
#   fit.glm = gbm(repeater ~ check_company + check_category + check_brand
#                 + check_brand*check_category*check_company
#                 + product_times_sc  + product_users_sc # +aov_sc +freq_sc
#                 + chain64 + chain152 + chain166
#                 + market1 + market15 + market21 + market96
#                 + aov + aov_factor + freq + freq_category + freq_product, #+ regularity,
#                 data=trainingset, distribution="adaboost", n.trees=500, shrinkage=0.01)#family=binomial)
  
  fit.glm = glm(repeater ~ check_company + check_category + check_brand
                + check_brand*check_category*check_company
                + product_times_sc  + product_users_sc # +aov_sc +freq_sc
                + chain64 + chain152 + chain166
                + market1 + market15 + market21 + market96
                + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc, #+ regularity,
                data=trainingset, family=binomial)
  
  
  # Testing
  pred = predict(fit.glm, testset, type="response")
#   pred = predict(fit.glm, testset, n.trees=500)
  real = testset$repeater
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")
summary(fit.glm)


# Auxiliar
fit.glm = glm(repeater ~ check_company + check_category + check_brand
              + check_brand*check_category*check_company
              + product_times_sc  + product_users_sc 
              + chain64 + chain152 + chain166
              + market1 + market15 + market21 + market96
              + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc,
              data=t, family=binomial)

write(names(fit.glm$coefficients[1:16]), file="") # list all the variables used
cat(names(fit.glm$coefficients),sep=", ") # list all the variables used (1 line)


# step selection
library(MASS)
step = stepAIC(fit.glm, direction="both")
summary(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")])
summary(scale(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")]))

ids_trans = c(TRANS[["id"]], TRANS_TEST[["id"]])
ids_trans = unique(ids_trans)


# Prediction --------------------------------------------------------------

# Import data
TEST = data.table(merge(test, offers[,c("offer","company","brand","category","offervalue")], all.x=T))
TRANS = fread("data/sample/transactions_test.csv")
setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))

# (~ 10min)
tTest = ExtractFeatures(TRANS, TEST)

pred = predict(fit.glm, tTest, type="response")
summary(pred)

# NA treatment
pred[is.na(pred)] = mean(train$repeater)

# analysis
selectedVars = c("offervalue","aov","aov_factor","freq","product_times","product_users","prize","category_prize",
                 "freq_category","freq_product","check_brand","check_category","check_company","prize_category_sc",
                 "discount","market1","market15","market21","market96","chain64","chain152","chain166")

summary(t[,selectedVars])
summary(tTest[,selectedVars])

# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=tTest$id, repeatProbability=pred), 
            file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")
