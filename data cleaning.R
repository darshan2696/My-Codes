
library(VIM)

train = read.csv("C:\\Users\\ethic\\Desktop\\train.csv")


length(unique(train$ID))  #So all the id's are unique

str(train)



sum(is.na(train$Ever_Married))
table(train$Ever_Married,useNA = "ifany")
train$Ever_Married[is.na(train$Ever_Married)] <- "Yes"

sum(is.na(train$Graduated))
table(train$Graduated,useNA = "ifany")
train$Graduated[is.na(train$Graduated)] <- "Yes"

sum(is.na(train$Profession))
table(train$Profession)
train$Profession = kNN(train, variable = "Profession" , k=3)
summary(train$Profession)
View(train$Profession)
train$Profession <- train$Profession[,6]


sum(is.na(train$Work_Experience))
train$Work_Experience[is.na(train$Work_Experience)] <- 0

sum(is.na(train$Family_Size))
str(train$Family_Size)
table(train$Family_Size,useNA = "ifany")
train$Family_Size[is.na(train$Family_Size)] <- 2

sum(is.na(train$Var_1))
table(train$Var_1,useNA = "ifany")
train$Var_1[is.na(train$Var_1)] <- "Cat_6"

getwd()
setwd("C:\\Users\\ethic\\Desktop")
write.csv(train,"train_clean_1.csv")


#Categorical to numerical conversion.

View(train)
train$Gender <- as.factor(train$Gender)
train$Gender <- as.numeric(train$Gender)
View(train$Gender)

train$Ever_Married <- as.factor(train$Ever_Married)
train$Ever_Married <- as.numeric(train$Ever_Married)
View(train$Ever_Married)

train$Graduated <- as.factor(train$Graduated)
train$Graduated <- as.numeric(train$Graduated)
View(train$Graduated)

train$Spending_Score <- as.factor(train$Spending_Score)
train$Spending_Score <- as.numeric(train$Spending_Score)
View(train$Spending_Score)

train$Var_1 <- as.factor(train$Var_1)
train$Var_1 <- as.numeric(train$Var_1)
View(train$Var_1)