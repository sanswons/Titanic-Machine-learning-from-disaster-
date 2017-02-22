library(rpart)
tree_model=rpart(Survived~Pclass+Fare+Sex+Adult+
            Embarked+Title+SibSp+Parch+CabinSection
            +Fare2,data= combi[1:891,],method = 'class')
plot(tree_model)
library('RGtk2')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_model)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")


rpart_test=combi[892:nrow(combi),]
rpart_test$Survived=NULL
pred_rpart <- predict(tree_model, rpart_test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_lr)
write.csv(submit, file = "submission.csv", row.names = FALSE)
