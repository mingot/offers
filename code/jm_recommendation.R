library(reshape2)
library(lsa)


S = TRANS[,list(times=.N), by=list(id, brand)]
setkey(S,id, brand)

# distance matrix ---------------------------------------------------------

p = 0.01 # percentage of users to choose to construct the distance matrix
users = unique(TRANS[["id"]])
users = users[sample(1:length(users), p*length(users))]

ptm = proc.time()
M = S[J(users)] # filter only selected users
M[, times:=1]
M = dcast(M, id ~ brand, value.var="times", fill=0)
M$id = as.character(M$id)
write.table(format(M,digits=4), "data_aux/user_brand.csv", sep=",", quote=F,row.names=F)
M = cosine(as.matrix(M[,!names(M) %in% "id"]))
proc.time() - ptm

# visualize it
require(lattice)
s = sample(1:nrow(M),100)
levelplot(M[s,s])

# save it
M[,1] = as.character(M[,1])
write.csv(format(M,digits=4),"data_aux/categories_cosine.csv")


# join with train ---------------------------------------------------------

names = as.numeric(colnames(M))
cats = data.frame(category=names)
cats$cat1 = apply(M, 1, function(x) names[order(-x)][2])
cats$cat1_val = apply(M, 1, function(x) sort(x, decreasing=TRUE)[2])
cats$cat2 = apply(M, 1, function(x)  names[order(-x)][3])
cats$cat2_val = apply(M, 1, function(x) sort(x, decreasing=TRUE)[3])
cats$cat3 = apply(M, 1, function(x)  names[order(-x)][4])
cats$cat3_val = apply(M, 1, function(x) sort(x, decreasing=T)[4])

S2 = merge(TRAIN[,list(id,category)], cats, by="category", all.x=T)
setnames(S2,"category","cat"); setnames(S2,"cat1","category")
S2 = merge(S2, S, by=c("id","category"), suffixes=c("","1"),all.x=T)
setnames(S2,"category","cat1"); setnames(S2,"cat2","category")
S2 = merge(S2, S, by=c("id","category"), suffixes=c("","2"), all.x=T)
setnames(S2,"category","cat2"); setnames(S2,"cat3","category")
S2 = merge(S2, S, by=c("id","category"), suffixes=c("","3"), all.x=T)
setnames(S2,"category","cat3"); setnames(S2,"cat","category")

NA2zero(S2)
S2[,cat1_val:=as.numeric(times>0)*cat1_val]
S2[,cat2_val:=as.numeric(times2>0)*cat2_val]
S2[,cat3_val:=as.numeric(times3>0)*cat3_val]
S2[,c("cat1","cat2","cat3","times","times2","times3"):=NULL, with=F]

TRAIN2 = merge(TRAIN, S2, by=c("id","category"), all.x=T) 

