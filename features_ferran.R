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
