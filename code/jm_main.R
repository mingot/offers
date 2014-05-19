
setwd("/Users/mingot/Projectes/kaggle/Offers")
train = read.table(file="data/original/trainHistory.csv", header=T, sep=",")
test = read.table(file="data/original/testHistory.csv", header=T, sep=",")
offers = read.table(file="data/original/offers.csv", header=T, sep=",")

# Sample transactions for test
transactions = read.table(file="data/sample/sample.txt", header=T, sep=",")
ids = unique(transactions$id)
trainSample = train[train$id %in% ids,]

# Conditional probabilities to success
train$repeater = 0
train$repeater[train$repeattrips>0] = 1
t = aggregate(train$repeater, by=list(train$offer), FUN=sum)
t2 = aggregate(train$repeater, by=list(train$offer), FUN=length)
t = merge(t,t2, by="Group.1", all.x=T)
t$prob = t$x.x/t$x.yn
names(t)[1] = "offer"
names(t)[4] = "prob"
t = t[,c("offer","prob")]
t = t[order(-t$prob),]
row.names(t) = NULL


t2 = data.frame(TEST)
t2[is.na(t2)] = 0

t[sample(1:nrow(t),10),6:16]
t2[sample(1:nrow(t),10),6:14]



# Extract Features --------------------------------------------------------

library(data.table)
library(bit64)
TRAIN = data.table(merge(train, offers[,c("offer","company","brand","category")], all.x=T))
TRANS = fread("data/sample/transactions_train.csv", header=T)
setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))

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

# average ticket
S = TRANS[,list(aov=sum(purchaseamount)), by=list(id, date)]
S = S[,list(aov=mean(aov)), by=id]
TRAIN = merge(TRAIN, S,by=c("id"), all.x=T)

# buy times per month
S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date)]
S = S[,month:=month(as.Date(S[["date"]]))]
S = S[,year:=year(as.Date(S[["date"]]))]
S = S[,list(times=length(id)),by=list(id,month,year)]

# data treatment
t = data.frame(TRAIN)
t$repeater = 0
t$repeater[t$repeattrips>0] = 1

# NA treatment
t[is.na(t)] = 0

# check for brands
t$check_brand = 0
t[t$BRANDquant!=0,"check_brand"] = 1
t$check_category = 0
t[t$CATquant!=0,"check_category"] = 1
t$check_company = 0
t[t$COMPquant!=0,"check_company"] = 1


t[,11:16] = scale(t[,11:16])
t$aov_sc = scale(t$aov)

# training ----------------------------------------------------------------
library(pROC)


k = 5 # Number of k-folds
id = sample(1:k,nrow(t),replace=TRUE)
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = t[id %in% list[-i],]
  testset = t[id %in% c(i),]
  
  # Training
#   fit.glm = glm(factor(repeater) ~ BRANDquant + BRANDamount + CATquant + CATamount + COMPquant + COMPamount, data=trainingset, family=binomial)
#   fit.glm = glm(repeater ~ BRANDamount + CATquant + COMPquant + COMPamount, data=trainingset, family=binomial)
  fit.glm = glm(repeater ~ check_company + check_category + check_brand + aov_sc, data=trainingset, family=binomial)
  
  # Testing
  pred = predict(fit.glm, testset, type="response")
  real = testset$repeater
  rmse =  sqrt(sum((pred - real) ^ 2))/length(real)
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")
fit.glm = glm(repeater ~ check_company + check_category + check_brand, data=t, family=binomial)

# step selection
library(MASS)
step = stepAIC(fit.glm, direction="both")
summary(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")])
summary(scale(t[,c("BRANDamount","CATquant","COMPquant","COMPamount")]))


# test import -------------------------------------------------------------

TEST = data.table(merge(test, offers[,c("offer","company","brand","category")], all.x=T))
TRANS = fread("data/sample/transactions_test.csv")
setnames(TRANS, c("id","chain","dept","category","company","brand","date","productsize","productmeasure","purchasequantity","purchaseamount"))

# convert to numeric some fields
set(TRANS, j="id", value=as.numeric(TRANS[["id"]]))
set(TRANS, j="company", value=as.numeric(TRANS[["company"]]))

# brand grouping
setkey(TRANS,id,brand)
setkey(TEST,id,brand)
S = TRANS[,list(BRANDquant=sum(purchasequantity), BRANDamount=sum(purchaseamount)), by=list(id,brand)]
TEST = merge(TEST, S, all.x=T)

# company grouping
setkey(TRANS,id,company)
setkey(TEST,id,company)
S = TRANS[,list(COMPquant=sum(purchasequantity), COMPamount=sum(purchaseamount)), by=list(id,company)]
TEST = merge(TEST, S, all.x=T)

# category grouping
setkey(TRANS,id,category)
setkey(TEST,id,category)
S = TRANS[,list(CATquant=sum(purchasequantity), CATamount=sum(purchaseamount)), by=list(id,category)]
TEST = merge(TEST, S, all.x=T)  

# average ticket
S = TRANS[,list(aov=sum(purchaseamount)), by=list(id, date)]
S = S[,list(aov=mean(aov)), by=id]
TEST = merge(TEST, S,by=c("id"), all.x=T)

# Apply the ML algorithm
t = data.frame(TEST)
t[is.na(t)] = 0
t$check_brand = 0
t[t$BRANDquant!=0,"check_brand"] = 1
t$check_category = 0
t[t$CATquant!=0,"check_category"] = 1
t$check_company = 0
t[t$COMPquant!=0,"check_company"] = 1
t$aov_sc = scale(t$aov)

pred = predict(fit.glm, t, type="response")
pred[pred>1]=1


# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=t$id, repeatProbability=pred), file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")

