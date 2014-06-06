
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


# ~10min
ptm = proc.time()
source('code/jm_features.R')
proc.time() - ptm


# training ----------------------------------------------------------------
library(pROC)
library(gbm)

k = 3 # Number of k-folds
id = sample(1:k,nrow(TRAIN),replace=TRUE)
data = as.data.frame(TRAIN)
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = data[id %in% list[-i],]
  testset = data[id %in% c(i),]
  
  # Training
  trainingset = trainingset[,!names(trainingset) %in% c("repeattrips","offerdate")]
  
  fit.gbm = gbm(repeater ~ check_company + check_category + check_brand
                + check_brand*check_category*check_company
                + product_times_sc  + product_users_sc # +aov_sc +freq_sc
                + chain64 + chain152 + chain166
                + market1 + market15 + market21 + market96
                + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc, #+ regularity,
                data=trainingset, distribution="adaboost", 
                n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)
  
#   fit.glm = glm(repeater ~ check_company + check_category + check_brand
#                 + check_brand*check_category*check_company
#                 + product_times_sc  + product_users_sc # +aov_sc +freq_sc
#                 + chain64 + chain152 + chain166
#                 + market1 + market15 + market21 + market96
#                 + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc, #+ regularity,
#                 data=trainingset, family=binomial)
  
  # Testing
#   pred = predict(fit.glm, testset, type="response")
  pred = predict(fit.gbm, testset, n.trees=500, type="response") 
  real = testset$repeater
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")

summary(fit.glm)
summary(fit.gbm)

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
pred =  predict(fit.gbm, tTest, n.trees=500, type="response") 
summary(pred)

# NA treatment
pred[is.na(pred)] = mean(train$repeater)

# analysis
selectedVars = c("offervalue","aov","aov_factor","freq","product_times","product_users","prize","prize_category_sc",
                 "freq_category","freq_product","check_brand","check_category","check_company","prize_category_sc",
                 "discount","market1","market15","market21","market96","chain64","chain152","chain166")

summary(t[,selectedVars])
summary(tTest[,selectedVars])


# save state --------------------------------------------------------------

write.table(t, file="data/sample/train.csv", row.names=F, quote=F, sep="," )
write.table(tTest, file="data/sample/test.csv", row.names=F, quote=F, sep="," )

t = read.table("data/sample/train.csv", sep=",", header=T)
tTest = read.table("data/sample/test.csv", sep=",", header=T)

# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=tTest$id, repeatProbability=pred), 
            file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")
