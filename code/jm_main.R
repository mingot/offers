library(data.table)
library(bit64)
setwd("/Users/mingot/Projectes/kaggle/Offers")


train = read.table(file="data/original/trainHistory.csv", header=T, sep=",")
test = read.table(file="data/original/testHistory.csv", header=T, sep=",")
offers = read.table(file="data/original/offers.csv", header=T, sep=",")

train$repeater = as.numeric(train$repeattrips>0)

# Extract Features --------------------------------------------------------

# Create data tables (~10 min)
TRAIN = data.table(merge(train, offers[,c("offer","company","brand","category","offervalue")], all.x=T))
TRANS = fread("data/sample/transactions_train.csv")

# ~30min
ptm = proc.time()
source('code/jm_features.R')
proc.time() - ptm

# join tris training
TRAIN_TRIS = fread("data/triskelion/train.csv", integer64="numeric")
n = paste("tris",names(TRAIN_TRIS), sep="_")
setnames(TRAIN_TRIS,names(TRAIN_TRIS), n)
setnames(TRAIN_TRIS, "tris_id","id")
TRAIN = merge(TRAIN, TRAIN_TRIS, by="id", all.x=T)
NA2zero(TRAIN)

# unused vars
unusedVars = c("check_brand","check_category","check_company","aov_sc","freq_sc","product_times_sc","product_users_sc"
       ,"tris_has_bought_company_q","tris_has_bought_category_q","tris_has_bought_brand_company"
       ,"tris_has_bought_company_q_180","tris_never_bought_category","tris_has_bought_category_q_60","aov_factor"
       ,"discount", "prize","product_times","product_users", "prize_category", "prize_category_sc","offervalue"
       ,"tris_offer_value"
       ,"tris_has_bought_brand_a_30","tris_has_bought_brand_a_180"
       ,"tris_has_bought_brand_a_90","tris_has_bought_company_a_60","tris_has_bought_company_a_90","tris_has_bought_category_a_60"
       ,"tris_has_bought_company_a_30","tris_has_bought_company_60","tris_has_bought_brand_q","tris_has_bought_category_a_90"
       ,"tris_has_bought_company_180","tris_has_bought_brand_q_90","tris_has_bought_category_q_90","tris_has_bought_brand_q_180"
       ,"tris_has_bought_category_60","tris_has_bought_company_30","tris_has_bought_company_q_30","tris_has_bought_company_q_90"
       ,"tris_has_bought_category_a_180","tris_has_bought_category_30","tris_has_bought_brand_90","tris_has_bought_category_180"
       ,"tris_has_bought_category_q_30","tris_has_bought_brand_180","tris_has_bought_brand_30","tris_has_bought_category_a"
       ,"tris_has_bought_brand_q_30","tris_has_bought_brand_60","tris_has_bought_brand_a","tris_has_bought_company_90"
       ,"check_product"
       ,"tris_offer_quantity","tris_has_bought_company_a","tris_has_bought_brand_company_category"
       ,"tris_has_bought_brand_company_category","tris_has_bought_brand_company_category","tris_has_bought_brand_company_category"
       ,"tris_has_bought_brand_company_category","tris_has_bought_brand_company_category","tris_has_bought_brand_company_category"
       ,"tris_has_bought_brand_company_category","tris_has_bought_brand_company_category")


# training ----------------------------------------------------------------
library(pROC)
library(gbm)

k = 3 # Number of k-folds
id = sample(1:k,nrow(TRAIN),replace=TRUE)
data = as.data.frame(TRAIN)
data$product = paste(data$category, data$brand, data$company, sep="_")
p = unique(data$product)
sel = sample(1:k,length(p),replace=T)
# data$check_product = data$check_company*data$check_brand*data$check_category
#data = data[,!names(data) %in% unusedVars]
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = data[data$product %in% p[sel!=i],]
  testset = data[data$product %in% p[sel==i],]
#   trainingset = data[id %in% list[-i],]
#   testset = data[id %in% c(i),]
  
  # Training
  
#   fit.gbm = gbm(repeater ~ check_company + check_category + check_brand + check_product
#                 + BRANDamount + CATamount + COMPamount
#                 + BRANDquant + CATquant + COMPquant
#                 #+ check_brand*check_category*check_company
#                 + product_times_sc  + product_users_sc # +aov_sc +freq_sc
#                 + chain64 + chain152 + chain166
#                 + market1 + market15 + market21 + market96
# #                 + cat1_val + cat2_val + cat3_val
#                 + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc, #+ regularity,
#                 data=trainingset, distribution="adaboost", 
#                 n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)

#   fit.gbm = gbm(repeater ~ . -tris_label -id -product -category -brand 
#                 -company -offer -chain -market -repeattrips -offerdate,
#               data=data, distribution="adaboost", 
#               n.trees=1500, shrinkage=0.5, interaction.depth=1, verbose=T)

  # Official Clean
  fit.gbm = gbm(repeater ~ check_company + check_brand
                + chain64 + chain152 + chain166 + market1 + market15 + market21 + market96 
                + aov + freq + freq_category + freq_product  
                + repeater_mean + tris_total_spend
                + tris_has_bought_brand_a_30 + tris_has_bought_brand_a_60 + tris_has_bought_brand_a_90 + tris_has_bought_brand_a_180 + tris_has_bought_brand
                + tris_has_bought_company_q_60 + tris_has_bought_category_a_180,
                data=trainingset, distribution="adaboost", 
                n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)

# fit.gbm = gbm(repeater ~ check_company + check_brand + check_product  
#               +chain64 + chain152 + chain166 + market1 + market15 + market21 + market96 
#               + aov + freq + freq_category + freq_product  
#               + repeater_mean + tris_total_spend + tris_has_bought_brand_company
#               + tris_has_bought_brand + tris_has_bought_brand_30 + tris_has_bought_brand_60 + tris_has_bought_brand_90 + tris_has_bought_brand_180
#               + tris_has_bought_brand_q + tris_has_bought_brand_q_30 + tris_has_bought_brand_q_60 + tris_has_bought_brand_q_90 + tris_has_bought_brand_q_180
#               + tris_has_bought_brand_a + tris_has_bought_brand_a_30 + tris_has_bought_brand_a_60 + tris_has_bought_brand_a_90 + tris_has_bought_brand_a_180
#               + tris_has_bought_category + tris_has_bought_category_30 + tris_has_bought_category_60 + tris_has_bought_category_90 + tris_has_bought_category_180
#               + tris_has_bought_category_q + tris_has_bought_category_q_30 + tris_has_bought_category_q_60 + tris_has_bought_category_q_90 + tris_has_bought_category_q_180
#               + tris_has_bought_category_a + tris_has_bought_category_a_30 + tris_has_bought_category_a_60 + tris_has_bought_category_a_90 + tris_has_bought_category_a_180
#               + tris_has_bought_company + tris_has_bought_company_30 + tris_has_bought_company_60 + tris_has_bought_company_90 + tris_has_bought_company_180
#               + tris_has_bought_company_q + tris_has_bought_company_q_30 + tris_has_bought_company_q_60 + tris_has_bought_company_q_90 + tris_has_bought_company_q_180
#               + tris_has_bought_company_a + tris_has_bought_company_a_30 + tris_has_bought_company_a_60 + tris_has_bought_company_a_90 + tris_has_bought_company_a_180,
#               data=trainingset, distribution="adaboost", 
#               n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)

  # TRAIN_TRIS
#   fit.gbm = gbm(tris_label ~ . - id - tris_offer_value -tris_offer_quantity -tris_never_bought_category,
#                 data=trainingset, distribution="adaboost", 
#                 n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)
  
# fit.gbm = gbm(tris_label ~ . -repeater -id -product -category -brand 
#               -company -offer -chain -market -repeattrips -offerdate + factor(aov_factor) - aov_factor,
#               data=trainingset, distribution="adaboost", 
#               n.trees=500, shrinkage=0.7, interaction.depth=1, verbose=T)
  
#   fit.glm = glm(repeater ~ check_company + check_category + check_brand
#                 + check_brand*check_category*check_company
#                 + product_times_sc  + product_users_sc # +aov_sc +freq_sc
#                 + chain64 + chain152 + chain166
#                 + market1 + market15 + market21 + market96
#                 + aov + aov_factor + freq + freq_category + freq_product + prize_category_sc, #+ regularity,
#                 data=trainingset, family=binomial)
  
    # Var stability
    if(i==1)
      features = summary(fit.gbm)
    else
      features = cbind(features, summary(fit.gbm))

  # Testing
#   pred = predict(fit.glm, testset, type="response")
  pred = predict(fit.gbm, testset, n.trees=500, type="response") 
  real = testset$tris_label
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")
rownames(features) = c()
format(features, digits=1)

features = data.frame()
features = cbind(features, summary(fit.gbm))

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


# Save state --------------------------------------------------------------

# TRAIN
write.table(TRAIN, file="data/intermediate/tablon_10_6_14.csv", sep=",", row.names=F, quote=F)
write.table(TRAIN, file="data/intermediate/tablon_12_6_14.csv", sep=",", row.names=F, quote=F)
TRAIN = fread("data/intermediate/tablon_12_6_14.csv", integer64="numeric")

# TEST
write.table(TEST, file="data/intermediate/test_10_6_14.csv", sep=",", row.names=F, quote=F)
write.table(TEST, file="data/intermediate/test_12_6_14.csv", sep=",", row.names=F, quote=F)
TEST = fread("data/intermediate/test_12_6_14.csv", integer64="numeric")

TRAIN[,n:=NULL,with=F]
TEST[,n:=NULL,with=F]

# Prediction --------------------------------------------------------------

# Import data
TRAIN = data.table(merge(test, offers[,c("offer","company","brand","category","offervalue")], all.x=T))
TRANS = fread("data/sample/transactions_test.csv")

# (~ 24min)
ptm = proc.time()
source('code/jm_features.R')
proc.time() - ptm

# join tris training
TEST_TRIS = fread("data/triskelion/test.csv", integer64="numeric")
n = paste("tris",names(TEST_TRIS), sep="_")
setnames(TEST_TRIS,names(TEST_TRIS), n)
setnames(TEST_TRIS, "tris_id","id")
TEST = merge(TEST, TEST_TRIS, by="id", all.x=T)
NA2zero(TEST)


pred =  predict(fit.gbm, TEST, n.trees=1500, type="response") 
summary(pred)



# submission --------------------------------------------------------------

d = as.character(Sys.time())
d = gsub(":","_", d)
d = gsub("-","_", d)
d = gsub(" ","_", d)
write.table(data.frame(id=TEST$id, repeatProbability=pred), 
            file=paste("data/submissions/sub",d,".csv",sep=""),row.names=F, quote=F, sep=",")
