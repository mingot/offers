

train = read.table(file="trainHistory", header=T, sep=",")
test = read.table(file="testHistory", header=T, sep=",")
offers = read.table(file="offers", header=T, sep=",")

# 1/0 instead of t/f
train$repeater = 0
train$repeater[train$repeattrips>0] = 1
test$repeater = 0
test$repeater[test$repeattrips>0] = 1


t = aggregate(train$repeater, by=list(train$offer), FUN=sum)
t2 = aggregate(train$repeater, by=list(train$offer), FUN=length)
t = merge(t,t2, by="Group.1", all.x=T)
t$prob = t$x.x/t$x.y
names(t)[1] = "offer"
names(t)[4] = "prob"
t = t[,c("offer","prob")]
t = t[order(-t$prob),]
row.names(t) = NULL
