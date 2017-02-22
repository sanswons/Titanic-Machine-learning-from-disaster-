# VARIABLE DESCRIPTIONS:
#       survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#       Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.


train=read.csv("Data/train.csv",stringsAsFactors = FALSE)
test=read.csv("Data/test.csv",stringsAsFactors = FALSE)
str(train)
str(test)

#
table(train$Pclass, train$Survived)
#class 1 majority survived and class 3 majority died
table(train$Sex, train$Survived)
#more women survived and more men died
table(train$SibSp,train$Survived)
#most passengers with 0 sibsp died
hist(train$Age)

#Feature Engineering
#Combine test and train

test$Survived=0
combi = rbind(train,test)
str(combi)
nrow(train)
nrow(test)

#remove na values
mean_fare=mean(combi$Fare,na.rm = TRUE)
combi$Fare[is.na(combi$Fare)]=mean_fare
#Add adult column
mean_age=mean(combi$Age,na.rm = TRUE)
combi$Age[is.na(combi$Age)]=mean_age
combi$Adult=combi$Age>0 & combi$Age<=18
combi$Adult

#Add title column

combi$Title=sapply(combi$Name,function(x){strsplit(x,' ')[[1]][2]})
table(combi$Title, combi$Survived)

combi$Title=as.factor(combi$Title)
combi$Embarked=as.factor((combi$Embarked))
combi$Sex=as.factor(combi$Sex)

combi$CabinSection=substr(combi$Cabin[1:1309],0,1)
combi$CabinSection=as.factor(combi$CabinSection)

combi$Fare2[combi$Fare<20]=1
combi$Fare2[combi$Fare>=20 & combi$Fare<30]=2
combi$Fare2[combi$Fare>=30]=3
table(combi$Fare2)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname=sapply(combi$Name,function(x){strsplit(x,' ')[[1]][1]})
combi$FamilyID=paste(as.character(combi$FamilySize),
                     combi$Surname,sep = '')
combi$FamilyID[combi$FamilySize<=2]='Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

combi$TicketID=substr(combi$Ticket[1:1309],0,1)
table(combi$TicketID)
str(combi)
combi$TicketID=as.factor(combi$TicketID)
