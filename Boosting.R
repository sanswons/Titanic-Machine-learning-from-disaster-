library(fastAdaboost)
str(combi)
combi$Survived=as.factor(combi$Survived)
boost=ada(as.factor(Survived)~Pclass + Sex + Age  + Fare + Adult+
                Embarked + CabinSection + Title + FamilySize +
                TicketID  +FamilyID2,
               data=combi[1:891,])
adb_test=combi[892:nrow(combi),]
adb_test$Survived=NULL
predBoost=predict(boost,newdata=adb_test)
print (predBoost)
table(predBoost)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predBoost)
write.csv(submit, file = "submission.csv", row.names = FALSE)


#XGBOOSt
xcombi=subset(combi,select = c("Pclass","Sex",'Age','SibSp','Parch'
                               ,'Fare','FamilySize','TicketID','Survived'))
Xtrain=xcombi[1:891,]
Xtest=xcombi[892:nrow(combi),]
Xtest$Survived=NULL

library(xgboost)

xgboost(data =Xtrain,label = Xtrain$Survived)


