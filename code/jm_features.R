


set(TRANS, j="id", value=as.numeric(TRANS[["id"]]))
set(TRANS, j="company", value=as.numeric(TRANS[["company"]]))

# Very fast NA values to zero
NA2zero = function(DT) {  
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

# Frequency vars ----------------------------------------------------------

FreqVars = function(TRANS, TRAIN, name){
  # Returns times/month the user goes shopping variable "name" (brand, company, category)
  #     name="category"
  #   name="product"
  S = TRANS[,list(value=sum(purchaseamount)), by=c('id',name,'date')]
  S$value[S$value!=0]=1
  S = S[, month:=substr(S[["date"]],1,7)] # extract month-year
  S = S[, times:=sum(value), by=c("id","month",name)] # group by month
  S = S[, list(aux=mean(times)), by=c("id",name)] # mean per user
  NA2zero(S)
  
  setnames(S,c("id",name, paste("freq", name, sep = "_")))
  
  TRAIN = merge(TRAIN, S, by = c("id",name),all.x=T) 
  NA2zero(TRAIN)
  return(TRAIN)
}
# General vars ------------------------------------------------------------

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

# Repeater
S = TRANS[,list(repeater_mean = .N), by=list(id,category,brand,company)]
S = S[,list(repeater_mean = mean(repeater_mean)), by = list(id)]
TRAIN = merge(TRAIN, S, by=c("id"), all.x=T)

# frequency (purchases per month)
cat("Frequency...\n")
S = TRANS[,list(aov=length(purchaseamount)), by=list(id, date)] #group by daily transactions
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
cat("Price...\n")
S = TRANS[purchaseamount>0 & purchasequantity>0,c("purchaseamount","purchasequantity","brand","company","category"), with=F] # remove negative transactions
S = S[,prize:=purchaseamount/purchasequantity]
S = S[,list(prize=mean(prize), prize_sd=sd(prize), prize_ferran=sd(prize)/mean(prize)), by=list(brand,company,category)]
S = S[,prize_category:=mean(prize), by=list(category)]
TRAIN = merge(TRAIN, S, by=c("brand","company","category"), all.x=T)

cat("Freq category...\n")
TRAIN = FreqVars(TRANS,TRAIN,"category")
cat("Freq product...\n")
TRAIN[, product := paste(brand, category, company,sep = "_")]
TRANS[, product := paste(brand, category, company,sep = "_")]
TEST = FreqVars(TRANS,TEST,"product")

NA2zero(TRAIN)
rm(TRANS)
# Cleaning ------------------------------------------------------------------

cat("Cleaning variables...\n")

# check for transaction history
TRAIN[, check_brand := as.numeric(BRANDquant!=0)]
TRAIN[, check_category := as.numeric(CATquant!=0)]
TRAIN[, check_company := as.numeric(COMPquant!=0)]
TRAIN[, check_product := check_brand*check_category*check_company]

# Scaling
# t[,11:16] = scale(t[,11:16])
TRAIN[, aov_sc := scale(aov)]
TRAIN[, freq_sc := scale(freq)]
TRAIN[, product_times_sc := scale(product_times)]
TRAIN[, product_users_sc := scale(product_users)]
TRAIN[, prize_category_sc := prize/prize_category]

# priors to known offers
# t = merge(t, offPrior, all.x=T)

l = c(64,152,166)
var = "chain"
for(v in l)
#   t[,paste(var,v,sep="")] = as.numeric(t[,var]==v)
  TRAIN[,paste(var,v,sep="") := as.numeric(TRAIN[[var]]==v), with=F]

l = c(1,15,21,96)
var = "market"
for(v in l)
  TRAIN[,paste(var,v,sep="") := as.numeric(TRAIN[[var]]==v), with=F]

# discount
TRAIN[, discount := offervalue/prize]

# NA treatment
NA2zero(TRAIN)

