str(combi)
logit=glm(Survived~Pclass+Fare+Sex+Adult+Embarked+Title+SibSp+Parch+CabinSection+Fare2, data=combi[1:891,],method="glm.fit",family="gaussian")
summary(logit)
str(lgtest)
lgtest$Survived=NULL
lgtest=combi[892:nrow(combi),]
pred_lr=predict(logit,newdata = lgtest,type = "link")
pred_lr
pred_lr[pred_lr>=0.5]=TRUE
pred_lr[pred_lr<0.5]=FALSE
submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_lr)
write.csv(submit, file = "submission.csv", row.names = FALSE)
table(pred_lr)
