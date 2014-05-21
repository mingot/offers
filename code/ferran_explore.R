source("dataVisualization.R")
library(pROC)

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
TRANS = TRANS[TRANS$id%in%TRAIN$id,]
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



head(TRANS)

# freq
TRANS = merge(TRANS,TRAIN[,which(names(TRAIN) %in% c('id','offerdate')), with = FALSE], by = "id", all.x = TRUE)


# AOV
S = TRANS[,list(aov=sum(purchaseamount)), by=list(id, date)]
S = S[,list(aov=mean(aov)), by=id]
TRAIN = merge(TRAIN, S, by="id", all.x=T)
# Outliers treatment
TRAIN$aov[TRAIN$aov > 300] = 300
TRAIN$aov_factor= cut(TRAIN$aov,breaks = c("-100","30","60","1000"), labels = c("low",
                                                                              "medium","high"))

# frequency (times per month)
S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date)] #group by dayly transactions
S = S[, month:=substr(S[["date"]],1,7)] # extract month-year
S = S[, times:=.N, by=list(id,month)] # group by month
S = S[, list(freq=mean(times)), by=id] # mean per user
TRAIN = merge(TRAIN, S, by="id", all.x=T)

# frequency (times per month) last 3 months
TRANS$diffdate = as.numeric(as.Date(TRANS$offerdate) - as.Date(TRANS$date))
nummonths = 3
TRANS$timedummy = 0; TRANS$timedummy[TRANS$diffdate < 30*nummonths] = 1
S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date,timedummy)] #group by dayly transactions
S = S[, list(freq_new=sum(timedummy)), by=id] # times per user
setnames(S,c("id",paste("log_freq_last",nummonths,sep = "_")))
# log_transformation
S[,2] = log(S[,2]+1)
TRAIN = merge(TRAIN, S, by="id", all.x=T)
TRANS$timedummy = NULL

# Loyalty variables

ShareFeatures = function(TRANS,TRAIN,name){
  # Returns share of amount, quantity and freq by "name"
  setkeyv(TRANS,c('id',name))
  setkeyv(TRAIN,c('id',name))
  
  S = TRANS[,list(quant=sum(purchasequantity), amount=sum(purchaseamount)), by=c('id',name)]
  S[is.na(S)] = 0
  setnames(S,c('id',name,paste(name,'quant',sep = "_"), paste(name,'amount',sep = "_")))
  TRAIN = merge(TRAIN, S, by = c("id",name),all.x=T) 
  TRAIN[is.na(TRAIN)] = 0
  
  setkey(TRANS,id)
  setkey(TRAIN,id)
  
  S = TRANS[,list(quantTotal=sum(purchasequantity),amountTotal=sum(purchaseamount)), by=list(id)]
  TRAIN = merge(TRAIN, S, all.x=T) 
  TRAIN$a= TRAIN[,which(names(TRAIN)%in%c(paste(name,'quant',sep = "_"))),with = FALSE]/TRAIN[,which(names(TRAIN)%in%c("quantTotal")),with = FALSE]
  setnames(TRAIN, c(names(TRAIN)[1:(length(names(TRAIN))-1)], paste("sh_quant", name, sep = "_")))
  TRAIN$a = TRAIN[,which(names(TRAIN)%in%c(paste(name,'amount',sep = "_"))),with = FALSE]/TRAIN[,which(names(TRAIN)%in%c("amountTotal")),with = FALSE]
  setnames(TRAIN, c(names(TRAIN)[1:(length(names(TRAIN))-1)], paste("sh_amount", name, sep = "_")))
  
  TRAIN$amountTotal = NULL; TRAIN$quantTotal = NULL; 
  a = TRAIN[,which(names(TRAIN)%in%c(paste(name,'quant',sep = "_"))),with = FALSE];
  

  TRAIN = merge(TRAIN, S, by = c("id",name),all.x=T) 
  TRAIN[is.na(TRAIN)] = 0
  
  
  return(TRAIN)
}

FreqVars = function(TRANS, TRAIN, name){
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

TRAIN = FreqVars(TRANS,TRAIN,"category")

TRAIN$product = paste(TRAIN$brand,TRAIN$category,TRAIN$company,sep = "_")
TRANS$product = paste(TRANS$brand,TRANS$category,TRANS$company,sep = "_")
TRAIN = FreqVars(TRANS,TRAIN,"product")

auc(TRAIN$repeater, TRAIN$freq_category)


S = TRANS[,list(value=sum(purchaseamount)), by=c('id','date')]
S$value = NULL
S2 = S
S2$ones = 1
S2 = S2[,list(times = sum(ones)), by = "id"]
S2 = S2[order(id),]; S = S[order(id,date),];

S$rank = sequence(S2$times)

S2 = S
S2$rank = S$rank + 1
S = merge(S,S2,by = c("id","rank"), all.x = TRUE)
S$diff = as.numeric(as.Date(S$date.x) - as.Date(S$date.y))
S$rank = NULL; S$date.x = NULL; S$date.y = NULL
S = S[,list(regularity = var(diff,na.rm = TRUE)), by = "id"]
S[is.na(S)] = 0
S$regularity = sqrt(S$regularity)

TRAIN = merge(TRAIN,S,by = "id", all.x = TRUE)

auc(TRAIN$repeater, TRAIN$regularity)