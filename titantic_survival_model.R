# VARIABLE DESCRIPTIONS:
# survival        Survival
#                 (0 = No; 1 = Yes)
# pclass          Passenger Class
#                 (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
#                 (C = Cherbourg; Q = Queenstown; S = Southampton)

# SPECIAL NOTES:
# Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

# set up dataframes
setwd("/Users/colbysong/Desktop/kaggle")
trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

# 1. Find variables related to what we are trying to predict
# A. Gender
# lets explore whether gender effects survival
sex_survival <- table(trainData$Survived, train$Sex)
sex_survival_prop <- prop.table(sex_survival, 2)
# Visualize 
barplot(sex_survival_prop, xlab = "Gender", ylab = "Survival Rate")
# Conclusion: Gender plays a role in predicting survival with females more likely to survive

# B. Passenger Class
# lets explore whether passenger class effects survival
Pclass_survival <- table(trainData$Survived, train$Pclass)
Pclass_survival_prop <- prop.table(Pclass_survival, 2)
# Visualize
barplot(Pclass_survival_prop, xlab = "Passenger Class", ylab = "Survival Rate")
# Conclusion: Passenger class plays a role in predicting survival with upper class (1) passegners
# being more likely to survive

# C. Age
# Lets explore whether age has an effect on surivival
# 2. Cleaning and modifying data and feature enigneering 
summary(trainData$Age)
# Note that there are 177 NAs (missing data), we will need to make an inference on these missing age values
# For simplicity, we will assume here that all the NA are simply the average age. 
# Note: there are many ways to fill in missing data, in this situation, you could calculate the average of 
# people by title and fill in the missing data according to the average age of each title.
# In our example, we will keep things simple and do abit of feature enigneering.
# Feature enigneering is really just applying human intuition and creativity to squeeze more values out of what is given to us.
# We have age information, but since there is a famous naval law of "woman and children gets off first", lets create an additional
# column of our data that indicates to us whether the person is a child or not. You might also suggest creating age bins and seperate
# age into groups of different ranges
trainData$Child <- 0
# person with age < 12 is considered a child
# the age cut off can be a factor for outcome, might have to play around it with a little bit
trainData$Child[trainData$Age < 12] <- 1
Age_survival <- table(trainData$Survived, trainData$Child)
Age_survival_prop <- prop.table(Age_survival, 2)
barplot(Age_survival_prop, xlab = "Child", ylab = "Survival Rate")
# Conclusion: Children seems to survive more

# More on feature enigneering: to further strength our model, we might enigneering some more variables such as Family size by added Parch 
# and Sibsp, Determine if someone is Mother by looking for title Mrs and having Parch greater than 1.

# At this point, we have enough information to predict survival in our test data, but there are alot more variables to explore such as ticket prices
# and maybe even embark port. This is way too much labour for us to do everything manual ourselves, looking at variable interactions
# and adjusting bin sizes for certain variables
# and this is where the power of R comes in. Its packages. Today, we will use machine learning to build decisions tree to do all the
# heavy lifting for us. 
library(rpart)
# explain decision tree, anova method will give us a continuous variable, class will give us 0 or 1
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = trainData, method = "class")
plot(fit)
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

# use our model to predict our test data
Prediction <- predict(fit, testData, type = "class")
submit <- data.frame(PassengerId = testData$PassengerId, Survived = Prediction)
write.csv(submit, file = "titantic_prediction_R_decision_tree.csv", row.names = FALSE)
