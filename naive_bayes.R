library("e1071")
nbtrain=combi[1:891,]
nb=naiveBayes(Survived~Pclass+Fare+Sex+Adult+Embarked
           +Title+SibSp+Parch,data=nbtrain,laplace = 1)
nbtest=combi[892:nrow(combi),]
nb$tables[1]

