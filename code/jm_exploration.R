
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


# Recommendation ----------------------------------------------------------
library(reshape2)
library(lsa)

S = TRANS[,list(times=.N), by=list(id, category)]
S[, times:=1]

rows=1000000
ptm = proc.time()
T = head(S, rows)
T = dcast(T, id~category, value.var="times", fill=0)
M = cosine(as.matrix(T[,!names(T) %in% "id"]))
proc.time() - ptm


require(lattice)
s = sample(1:nrow(M),100)
levelplot(M[s,s])
