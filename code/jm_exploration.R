
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


t = data.frame(TRAIN)
summary(t[,11:16])
# BRAND
head(S)

#offers model
aux = setNames(aggregate(train$repeater, by=list(train$offer), FUN=mean), c("offer", "prior"))
offersTrain = merge(offers, aux, all.y=T)
offersTest = offers[!(offers$offer %in% offersTrain$offer), ]


sum(offersTest$category %in% offersTrain$category)

offers.glm = lm(prior~offervalue, offersTrain)




## P(repeat|weekday)
d = setnames(aggregate(train$repeater, by=list(weekdays(as.Date(train$offerdate))), FUN=mean), c("weekday","prob"))
barplot(height=d$prob, legend.text=d$weekday)

## P(repeat|chain)
d = setnames(aggregate(train$repeater, by=list(train$chain), FUN=mean), c("chain","prob"))
d = merge(d, setnames(aggregate(train$repeater, by=list(train$chain), FUN=sum), c("chain","num")))
d[d$prob>0.5,]
barplot(height=d$prob)

## P(repeat |)



