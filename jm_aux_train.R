k = 3 # Number of k-folds
data = as.data.frame(TRAIN)
data$product = paste(data$category, data$brand, data$company, sep="_")
p = unique(data$product)
# sel = sample(1:k,length(p),replace=T)
# data$check_product = data$check_company*data$check_brand*data$check_category

aucs=c()
for (i in 1:k){
  trainingset = data[data$product %in% p[sel!=i],]
  testset = data[data$product %in% p[sel==i],]
  
  # Official Clean
  fit.gbm = gbm(repeater ~ check_company + check_brand
                + chain64 + chain152 + chain166 + market1 + market15 + market21 + market96 
                + aov + freq + freq_category + freq_product  
                + repeater_mean + tris_total_spend
                + tris_has_bought_brand + tris_has_bought_company + tris_has_bought_category,
                data=trainingset, distribution="adaboost", 
                n.trees=500, shrinkage=0.5, interaction.depth=1, verbose=T)
  
  # Var stability
  if(i==1)
    features = summary(fit.gbm)
  else
    features = cbind(features, summary(fit.gbm))
  
  # Testing
  pred = predict(fit.gbm, testset, n.trees=500, type="response") 
  real = testset$tris_label
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")
rownames(features) = c()
format(features, digits=1)


