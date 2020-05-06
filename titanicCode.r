library(ggplot2)
library(caret)
library(dplyr)
library(ggthemes)

setwd("/Titanic")
download <- read.csv("trainData.csv")

str(download)

summary(download$Age)

summary(download$Fare)

summary(download$Embarked)

# convert train dataset
download$Survived <- factor(download$Survived)
download$Pclass <- factor(download$Pclass)

str(download$Survived)
str(download$Pclass)

# fix NAs for Age
avg <- mean(download$Age, na.rm = TRUE)
download$Age <- replace(download$Age,is.na(download$Age),avg)

# fix NAs for Fare
avg <- mean(download$Fare, na.rm = TRUE)
download$Fare <- replace(download$Fare,is.na(download$Fare),avg)


summary(download$Age)

summary(download$Fare)

download$Embarked[!(download$Embarked %in% c('C','Q','S'))] <- 'S'
summary(download$Embarked)

# create partition on test data
set.seed(1)
index <- createDataPartition(download$Survived,times = 1,p=0.8,list=FALSE)
train <- download[index,]
test <- download[-index,]

# check the dimesions
dim(download)
dim(train)
dim(test)


table(train$Survived)


num <- table(train$Sex,train$Survived)
pct <- round(prop.table(table(train$Sex,train$Survived),1),3) * 100
tbl <- cbind(num,pct)
colnames(tbl) <- c('Died','Survived','Died (%)','Survived (%)')
tbl


kids <- train %>% filter(Age<=15)
num <- table(kids$Sex,kids$Survived)
pct <- round(prop.table(table(kids$Sex,kids$Survived),1),3) * 100
tbl <- cbind(num,pct)
colnames(tbl) <- c('Died','Survived','Died (%)','Survived (%)')
tbl



tbl <- train %>%
  group_by(Sex) %>%
  summarise(Count = n())



tbl_ratio <- train %>%
  group_by(Sex, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))


train %>%
  ggplot() +
  geom_bar(aes(x = Sex, fill = Survived)) +
  geom_text(data = tbl, 
            aes(x = Sex, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = tbl_ratio, 
             aes(x = Sex, y = Count, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Survival Rate by Gender") +
  scale_x_discrete(name= "Gender") +
  scale_y_continuous(name = "Passengers") +
  scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived"))  


## 3.2 Pclass
# create tbl and ratio for geom text and geom label
tbl <- train %>%
  group_by(Pclass) %>%
  summarise(Count = n())

tbl_ratio <- train %>%
  group_by(Pclass, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

# ggplot to put everything in one plot
train %>%
  ggplot() +
  geom_bar(aes(x = Pclass, fill = Survived)) +
  geom_text(data = tbl, 
            aes(x = Pclass, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = tbl_ratio, 
             aes(x = Pclass, y = Count, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Survival Rate by Passenger Class") +
  scale_x_discrete(name= "Pclass") +
  scale_y_continuous(name = "Passengers") +
  scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived"))  

## 3.3 Family
# group to determine family/single
train$FamilySize <- train$Parch + train$SibSp + 1

# work out the composition of the data
a <- prop.table(table(train$FamilySize)) * 100
b <- table(train$FamilySize)
rbind(Count = b,Pctg = round(a,1))

num <- table(train$FamilySize,train$Survived)
pct <- round(prop.table(table(train$FamilySize,train$Survived),1),3)*100
tbl <- cbind(num,pct)
colnames(tbl) <- c('Died','Survived','Died %','Survived %')
tbl


ggplot(data = train, aes(x=FamilySize)) + geom_bar(aes(fill=Survived)) + labs(x='Family Size', y='Tot Passengers', title='Family Size Survival') + theme_economist_white()

train %>% filter(FamilySize>7) %>% select(Name,Sex,Age, FamilySize, Pclass) %>% arrange(Name)

TotalPassenger <- table(train$Embarked)
Pctg <- round(prop.table(table(train$Embarked)),3)*100
Pctg <- Pctg[-1]
TotalPassenger <- TotalPassenger[-1]
rbind(TotalPassenger,Pctg)


train %>% filter(Age==28 & Sex=='female' & Fare < 10) %>% select(Survived,Pclass,Name,Sex,Embarked,FamilySize) %>% arrange(desc(Embarked))

train %>% ggplot(aes(x=Pclass, fill=Survived)) + geom_bar() + facet_wrap(~Embarked) + theme_economist_white() + labs(title="Passenger Class by Embarked")

p <- train %>% 
  ggplot(aes(x = Age, fill=Survived)) 

p + geom_density(alpha=0.5) + facet_wrap(~Sex) + labs(title="Age Feature") + theme_economist_white()


tmp <- train %>% filter(Age<15 & Sex=='female') 
table(tmp$FamilySize,tmp$Survived)

tmp %>% filter(FamilySize>4) %>% select(Name,Pclass,FamilySize,Sex,Age) %>% arrange(desc(FamilySize,Name))


train$AgeGrp[train$Age<=15] <- 'Kids'
train$AgeGrp[train$Age>15 & train$Age<=59] <- 'Adults'
train$AgeGrp[train$Age>59] <- 'Seniors'
train$AgeGrp <- factor(train$AgeGrp)


a <- table(train$AgeGrp)
b <- prop.table(table(train$AgeGrp)) * 100
rbind(Count=a,Pctg=round(b,1))

train %>% ggplot(aes(x=AgeGrp, fill=Survived)) + geom_bar() + theme_economist_white() + labs(title="Survival by Age Group")

## 3.6

train %>% ggplot(aes(x = Pclass, y = Fare, fill=Pclass)) + geom_boxplot() + facet_grid(~Survived) + theme_classic()

s <- train %>% filter(Pclass==3) %>% .$Fare
summary(s)

train %>% filter(Pclass==3 & Fare>15.50) %>% select(Name, FamilySize, Age, Embarked) %>% arrange(desc(FamilySize,Name))


p <- train %>% filter(Fare<200) %>%
  ggplot(aes(x = Fare, fill=Survived)) 

p + geom_density(alpha=0.5) + labs(title="Fare Grouping") + theme_economist_white()



p <- train %>% filter(Fare>200) %>%
  ggplot(aes(x = Fare, fill=Survived)) 

p + geom_density(alpha=0.5) + labs(title="Fare Grouping") + theme_economist_white()


train$Fare2[train$Fare<10] <- 'Low Fare'
train$Fare2[train$Fare>=10 & train$Fare<=200] <- 'Normal Fare'
train$Fare2[train$Fare>200 & train$Fare<=300] <- 'Outlier1'
train$Fare2[train$Fare>300] <- 'Outlier2'


## 4 Machine Learning
set.seed(7)

fit.rf <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="rf")
fit.rpart <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="rpart")
fit.glm <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="glm")
fit.knn <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="knn")
fit.xgbTree <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="xgbTree")
fit.SVM <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="svmRadial")


set.seed(7)

results <- resamples(list(DecisionTree=fit.rpart,RandomForest=fit.rf,XgBoost=fit.xgbTree,  KNN=fit.knn, SupportVector=fit.SVM, GLM=fit.glm))
summary(results)


dotplot(results)

print(fit.rf)

ggplot(fit.rf, highlight = TRUE)

seq(2,5,1)

# Machine learning one using 7 fold validation
library(caret)
train$Survived <- factor(train$Survived)
set.seed(7)

# create 7 fold validation
control <- trainControl(method="cv", number=5, p=0.8)
metric <- "Accuracy"

# Random Forest
fit.rf_final <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(2, 5, 1)), trControl=control)

# print model
print(fit.rf_final)

ggplot(fit.rf_final, highlight = TRUE)

fit.rf_final$finalModel



train$Survived2 <- predict(fit.rf_final,train)
train$Survived2 <- factor(train$Survived2)
confusionMatrix(train$Survived2,train$Survived)

test$FamilySize <- test$Parch + test$SibSp + 1


test$AgeGrp[test$Age<=15] <- 'Kids'
test$AgeGrp[test$Age>15 & test$Age<=59] <- 'Adults'
test$AgeGrp[test$Age>59] <- 'Seniors'
test$AgeGrp <- factor(test$AgeGrp)

test$Fare2[test$Fare<10] <- 'Low Fare'
test$Fare2[test$Fare>=10 & test$Fare<=200] <- 'Normal Fare'
test$Fare2[test$Fare>200 & test$Fare<=300] <- 'Outlier1'
test$Fare2[test$Fare>300] <- 'Outlier2'


test$Survived2 <- predict(fit.rf_final,test)
test$Survived2 <- factor(test$Survived2)
confusionMatrix(test$Survived2,test$Survived)




