setwd("C:\\Users\\anupama25\\GitHub\\Kaggle_Titanic_Dataset")
train=read.csv("train.csv")
test = read.csv("test.csv")



summary(train)
is.na(train)
temp=summary(is.na(train))
as.table(temp)
typeof(as.table(temp))
typeof(train)
#View(train)

miss_values = apply(train, 2, function(x) (sum(is.na(x))/length(x))*100)
miss_values

#age values are missing, replacing them with average age value
train$Age[which(is.na(train$Age))] <- mean(train$Age, na.rm = TRUE)
miss_values = apply(train, 2, function(x) (sum(is.na(x))/length(x))*100)
miss_values

str(train)

train_model = train
test_model = test

#removing PassengerId, Name, ticket and Cabin
colnames(train_model)
colnames(test_model)

Y_train = train_model$Survived
train_model= train_model[,-2] # same predictors for test and train


colnames(train_model)
colnames(test_model)

del = c(1,3,8,10)
train_model = train_model[,-del]
colnames(train_model)
test_model = test[,-del]
colnames(test_model)
apply(test_model, 2, function(x) (sum(is.na(x))/length(x))*100)

#replacing missing values for test data
test_model$Age[which(is.na(test_model$Age))] <- mean(test_model$Age, na.rm = TRUE)
test_model$Fare[which(is.na(test_model$Fare))] <- mean(test_model$Fare, na.rm = TRUE)
#------------------------------------------------------------fare can be replace by the relative class fare





# adding dummy
male= as.numeric(train_model$Sex=="male") 

#sum(male)
#summary(train_model$Sex)

#colnames(train_model)
#removing Sex and adding dummy "male'

train_model=cbind(train_model[,-2], male)
male_test = as.numeric(test_model$Sex=="male") 
test_model=cbind(test_model[,-2], male_test)

#instead of having 2 columns as Parch and Sibsp we can combine to know if they are with family

family = as.numeric(train_model$SibSp+train_model$Parch >0)
del1 = c(4,5)
train_model=cbind(train_model[,-del1], family)

family_test = as.numeric(test_model$SibSp+test_model$Parch >0)
del1 = c(4,5)
test_model=cbind(test_model[,-del1], family_test)

#creating dummies for embarked

C = as.numeric(train_model$Embarked=="C")
Q = as.numeric(train_model$Embarked=="S")
train_model=cbind(train_model[,-4], C, Q)


C= as.numeric(test_model$Embarked=="C")
Q = as.numeric(test_model$Embarked=="S")
test_model=cbind(test_model[,-4], C, Q)

str(train_model)
str(test_model)


####BUILDING LOGISTIC REGRESSION MODEL

# adding Survived Column back to train_model
train_model=cbind(train_model, Y_train)
#divide train in train n test to check model accuracy

part = round(.7* nrow(train_model),0)
train_model_train = train_model[1:part,]
train_model_test =  train_model[(part+1):nrow(train_model),]

logmodel <- glm(Y_train ~., family = binomial(link = "logit"), data = train_model_train)
summary(logmodel)



resultPredict <- predict(logmodel, newdata = train_model_test)
resultPredict <- ifelse(resultPredict>0.5,1,0)

error = mean(resultPredict!=train_model_test$Y_train)
print(paste('Accuracy',1-error))
