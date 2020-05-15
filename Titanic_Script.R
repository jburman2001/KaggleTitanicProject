#Set the working directory to where Titanic Files are located
setwd("C:/Users/nicho/OneDrive/Desktop/2019-2020/Data Science/Kaggle Titanic/Titanic Competition")

#Have RStudio read the csv files
ship.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
ship.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#Combine them
ship.train$IsTrainSet <- TRUE
ship.test$IsTrainSet <- FALSE

ship.test$Survived <- NA

ship.full <- rbind(ship.train, ship.test)

#Clean data in ship.full for Embarked
ship.full[ship.full$Embarked == '', "Embarked"] <- 'S'

#Clean data in ship.full for Age
age.median <- median(ship.full$Age, na.rm = TRUE)
ship.full[is.na(ship.full$Age), "Age"] <- age.median

#Clean data in ship.full for Fare using Predictve Model

upper.boundary <- boxplot.stats(ship.full$Fare)$stats[5]
outlier.filter <- ship.full$Fare < upper.boundary
ship.full[outlier.filter,]

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = ship.full[outlier.filter,]
)

fare.row <- ship.full[
  is.na(ship.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
  ]

fare.predictions <- predict(fare.model, newdata = fare.row)
ship.full[is.na(ship.full$Fare), "Fare"] <- fare.predictions
#Categorial casting
ship.full$Pclass <- as.factor(ship.full$Pclass)
ship.full$Sex <- as.factor(ship.full$Sex)
ship.full$Embarked <- as.factor(ship.full$Embarked)

#Throw this back into the .csv files
ship.train<- ship.full[ship.full$IsTrainSet == TRUE,]
ship.test <- ship.full[ship.full$IsTrainSet == FALSE,]

#Handle Survived Case
ship.train$Survived <- as.factor(ship.train$Survived)

#Build Predictive Model Using Formulas and Random Forests
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

ship.model <- randomForest(formula = survived.formula, data = ship.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(ship.test))

#Features
features.equation <- "Pclass + Sex + Age + SibSp + Fare + Embarked"
Survived <- predict(ship.model, newdata = ship.test)

#Turning results back out into a .csv file
PassengerId <- ship.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="kaggle_submission.csv", row.names = FALSE)
