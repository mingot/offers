

tablon = t[,c("repeater","market","check_company","check_category","check_brand")]

var = "market"
for(v in unique(tablon[,var]))
  tablon[,paste(var,v,sep="")] = as.numeric(tablon[,var]==v)



k = 3 # Number of k-folds
id = sample(1:k,nrow(tablon),replace=TRUE)
list = 1:k
aucs=c()
for (i in 1:k){
  trainingset = tablon[id %in% list[-i],]
  testset = tablon[id %in% c(i),]
  
  # Training  
  fit.glm = glm(repeater ~ check_company + check_category + check_brand + market34 + 
                  market21 + market35 + market4 + market22 + market2 + market9 + 
                  market11 + market1 + market8 + market14 + market7 + market10 + 
                  market15 + market33 + market20 + market27 + market26 + market16 + 
                  market37 + market18 + market23 + market24 + market6 + market17 + 
                  market96,
                  data=trainingset, family=binomial)
  
  # Testing
  pred = predict(fit.glm, testset, type="response")
  real = testset$repeater
  aucs = c(aucs,auc(real, pred))
  cat("auc:",auc(real, pred),"\n")
}
cat("mean auc:", mean(aucs),"sd:",sd(aucs),"\n")

# STEP
step = stepAIC(fit.glm, direction="both")



