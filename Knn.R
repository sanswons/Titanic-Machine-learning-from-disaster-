normalize <- function(x) {
      num <- x - min(x,na.rm = TRUE)
      denom <- max(x,na.rm = TRUE) - min(x,na.rm = TRUE)
      return (num/denom)
}

knncombi=combi[c('Pclass','Sex','Age','SibSp','Parch','Fare','Adult')]
str(knncombi)
knncombi$Sex[knncombi$Sex=='male']=0
knncombi$Sex[knncombi$Sex=='female']=1
knncombi$Adult[knncombi$Adult==FALSE]=0
knncombi$Adult[knncombi$Adult==TRUE]=1

knncombi$Sex=as.integer(knncombi$Sex)
knncombi_norm <- as.data.frame(lapply(knncombi, normalize))
knncombi_norm[is.na(knncombi_norm)]=0
str(knncombi_norm)
knntrain=knncombi_norm[1:891,]
knntest=knncombi_norm[892:1309,]
knntrainlabels=train$Survived
knntestlabels=test$Survived

pred=knn(train = knntrain,test=knntrain, cl=knntrainlabels,k=2,use.all = TRUE,prob = TRUE)
table(knntrainlabels,pred)
submit <- data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submit, file = "submission.csv", row.names = FALSE)
