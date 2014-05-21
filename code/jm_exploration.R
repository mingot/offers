
source('code/dataVisualization.R')

## P(repeat|weekday)
d = setnames(aggregate(train$repeater, by=list(weekdays(as.Date(train$offerdate))), FUN=mean), c("weekday","prob"))
barplot(height=d$prob, legend.text=d$weekday)

## P(repeat|chain)
d = setnames(aggregate(train$repeater, by=list(train$chain), FUN=mean), c("chain","prob"))
d = merge(d, setnames(aggregate(train$repeater, by=list(train$chain), FUN=sum), c("chain","num")))
d[d$prob>0.5,]
barplot(height=d$prob)

## P(repeat |)
dataVisualization(response="repeater", name="freq", data=t, type="numerical")
dataVisualization(response="repeater", name="product_users", data=t, type="numerical")
dataVisualization(response="repeater", name="offer_prior", data=t, type="numerical")
dataVisualization(response="repeater", name="aov", data=t, type="numerical")

dataVisualization(response="repeater", name="company", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="brand", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="category", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="market", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="chain", data=t, type="categorical", aes="points")
dataVisualization(response="repeater", name="offer", data=t, type="categorical", aes="points")

dep = TRANS[["dept"]]
length(unique(dep))
cat = TRANS[["category"]]
length(unique(cat))
comp = TRANS[["company"]]
length(unique(comp))

comp_train = unique(t$company)
comp_test = unique(tTest$company)

brand_train = unique(t$brand)
brand_test = unique(tTest$brand)


