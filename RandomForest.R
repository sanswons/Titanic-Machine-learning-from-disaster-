library("randomForest")
set.seed(60)
RF=randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                             Embarked + Title + FamilySize + FamilyID2,
                       data=combi[1:891,], 
                       importance=TRUE, 
                       ntree=2000)
varImpPlot(RF)

library(party)
set.seed(415)
rf_test=combi[892:nrow(combi),]
rf_test$Survived=NULL

RF <- cforest(as.factor(Survived) ~ TicketID+Pclass + Sex + Age+ Fare +
            Embarked+Adult+Title+FamilySize+FamilyID,
               data = combi[1:891,], 
               controls=cforest_unbiased(ntree=2000, mtry=3))
pred_rf <- predict(RF, rf_test,OOB=TRUE,type='response')
table(pred_rf)
submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_rf)
write.csv(submit, file = "submission.csv", row.names = FALSE)
 