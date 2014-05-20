dataVisualization<-function(response,name,data,type){
  feature = data[,names(data)%in%name] 
  output = data[,names(data)%in%response]
  
  if(type == "binomial"){
    m1 = mean(output[which(feature == 1)]);
    m2 = mean(output[which(feature == 0)]);
    means = c(m1,m2)
    
    barplot(means, names.arg = c("1","0"), xlab = name, ylab = paste("Probability", response),main = name)
  }else if(type == "numerical"){
    df = data.frame(feature,output)
    df = df[order(feature),]
    n =nrow(data)
    splits = 30
    ns = floor(n/splits)
    x1 = rep(0,splits)
    x2 = rep(0,splits)
    for(i in 1:splits){
      x1[i] = mean(df[((i-1)*ns+1):(i*ns),1])
      x2[i] = mean(df[((i-1)*ns+1):(i*ns),2])
    }
    
    xx1 = unique(x1)
    xx2 = rep(0,length(xx1))
    for(i in 1:length(xx1))
      xx2[i] = mean(x2[which(x1 == xx1[i])])
    
    plot(xx1,xx2, type = "l", col = "blue", xlab = name, ylab = paste("Probability", response), main = name)
  }else{
    
    l = levels(feature)
    means = rep(0, length(l))
    for(i in 1:length(means))
      means[i] = mean(output[which(feature == l[i])])
    
    barplot(means, names.arg = l, ylab = paste("Probability",response),xlab = name, main = name)
  }
}