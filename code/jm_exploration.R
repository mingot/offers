
source('code/dataVisualization.R')

## P(repeat|weekday)
d = setnames(aggregate(train$repeater, by=list(weekdays(as.Date(train$offerdate))), FUN=mean), c("weekday","prob"))
barplot(height=d$prob, legend.text=d$weekday)

## P(repeat|chain)
d = setnames(aggregate(train$repeater, by=list(train$chain), FUN=mean), c("chain","prob"))
d = merge(d, setnames(aggregate(train$repeater, by=list(train$chain), FUN=sum), c("chain","num")))
d[d$prob>0.5,]
barplot(height=d$prob)

## Individual visualitzations
dataVisualization(response="repeater", name="freq", data=t, type="numerical")
dataVisualization(response="repeater", name="product_users", data=t, type="numerical")
dataVisualization(response="repeater", name="offer_prior", data=t, type="numerical")
dataVisualization(response="repeater", name="aov", data=t, type="numerical")

dataVisualization(response="repeater", name="offer", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="company", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="brand", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="category", data=t, type="categorical", aes="points")

dataVisualization(response="repeater", name="market", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="chain", data=t, type="categorical", aes="points")


## Information present in test
var = "category"
fTrain = unique(t[,var])
fTest = unique(tTest[,var])
intersection = fTrain[fTrain %in% fTest]
cat("Train contains: ",length(fTrain)," unique ", var)
cat("Test contains: ",length(fTest)," unique ", var)
cat("They have an intersection of: ", length(intersection)," elements")
cat("That intersection represents: ",sum(tTest[,var] %in% intersection)*100/nrow(tTest),"% volume of test")


## differences test train
par(mfrow=c(1,2))
var = "prize"
hist(t[,var], main="train", xlab=var)
hist(tTest[,var], main="test", xlab=var)



# TRAIN vs TEST -----------------------------------------------------------

source('code/dataVisualization.R')

vars = as.character(summary(fit.gbm)$var)

i = "prize_category_sc"#vars[23]; i
par(mfrow=c(2,2))
trainVar = TRAIN[[i]]; trainVar = trainVar[trainVar < quantile(trainVar,0.99)]# & trainVar > quantile(trainVar,0.01)]
testVar = TEST[[i]]; testVar = testVar[testVar < quantile(trainVar,0.99)]# & testVar > quantile(testVar,0.01)]    
hist(trainVar,breaks = 10, main=i)
hist(testVar,breaks = 10, main=i)
boxplot(trainVar)
boxplot(testVar)
summary(trainVar)
summary(testVar)
par(mfrow=c(1,1))
dataVisualization(response="repeater", name=i, data=as.data.frame(TRAIN), type="numerical")

#binomial
trainVar = TRAIN[[i]]; 
testVar = TEST[[i]]; 
table(trainVar); table(testVar)
summary(trainVar); summary(testVar)
par(mfrow=c(1,1))
dataVisualization(response="repeater", name=i, data=as.data.frame(TRAIN), type="binomial")

# error q_60 --------------------------------------------------------------

vars = c("tris_has_bought_company","tris_has_bought_company_30","tris_has_bought_company_60","tris_has_bought_company_90","tris_has_bought_company_180")
TRAIN[sample(1:nrow(TRAIN),100), vars, with=F]

v  = TRAIN[["tris_has_bought_company_q_30"]] > TRAIN[["tris_has_bought_company_q_60"]]
sum(v)*100 /nrow(TRAIN)
head(TRAIN[v,c("id","brand","category","company",vars),with=F],100)

TRANS[id==86252 & brand==13474]
head(TRAIN_TRIS[["has_bought_brand_q_60"]],1000)


