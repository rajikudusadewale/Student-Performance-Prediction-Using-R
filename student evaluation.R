library('tidyverse')
library('data.table')
library('superml')
std_df <- read.table('student data.csv', header = TRUE, sep = ";")

View(std_df)
head(std_df, n = 3)
str(std_df)
summary(std_df)
#renaming few variables:
setnames(std_df, old = c("Pstatus", "Medu", "Fedu", "G1", "G2", "G3"), 
         new = c("parent_cohabitation_status", "mother_edu", "father_edu", 
                 "first_period_grade", "second_period_grade", "final_grade"))
str(std_df)

#checking for missing values:
sapply(std_df, function(x) sum(is.na(x), na.rm = TRUE)/ length(x) * 100)
library(Amelia)
missmap(std_df, main = "Missing Map")

#creating our target variable 
std_df$Success_Status <- NA
std_df$Success_Status[std_df$final_grade >= 10] <- 1
std_df$Success_Status[std_df$final_grade < 10] <- 0


# Exploratory data analysis
#grade distribution
ggplot(std_df, aes(x = final_grade))+
  theme_bw()+
  geom_histogram(binwidth = 0.5)+
  labs(y = "Students count",x = 'Grades', 
       title = "Student Grades Distribution")
#final grade and sex
ggplot(std_df, aes(y = final_grade, fill = sex))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "Grades", x = 'Sex',
       title = "Grade Distribution")

#grade rates with family support and sex
ggplot(std_df, aes(x = sex, y = final_grade, color = famsup))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "Grades", x = 'Sex',
       title = "Grade rate with family support & sex")
#grade rates with school support and sex
ggplot(std_df, aes(x = sex, y = final_grade, color = schoolsup))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "Grades", x = 'Sex',
       title = "Grade rate with school support & sex")

#abs & Sex
ggplot(std_df, aes(absences, fill = sex))+
  geom_bar()+
  labs(title = 'Variation in Male and Female Absences', x = 'Absences', y = 'Students count')
  
#Student Success Rates by Sex, Internet, and Studytime
ggplot(std_df, aes(x = final_grade, fill = sex))+
  theme_bw()+
  facet_wrap(internet~studytime)+
  geom_density(alpha=0.5)+
  labs(y = "Density count", x = "Grades", title = "Student Success Rates by Sex, Internet, and Studytime")

#g
ggplot(std_df, aes(x = absences, y = final_grade, color = sex))+
  geom_point()+
  labs(title = 'scatter plot of Final grade & Absences')


plot(std_df$absences, std_df$final_grade, pch = 16,cex=1.2 ,col = 'blue', 
     main = 'Grades plotted against Absences', xlab = 'Absences',ylab = 'Grades')
abline(lm(std_df$absences ~ std_df$final_grade))

lm(std_df$absences ~ std_df$final_grade)
# add to the word doc
# Call:
#   lm(formula = std_df$absences ~ std_df$final_grade)
# 
# Coefficients:
#   (Intercept)  std_df$final_grade  
# 5.2223             -0.1313 

######################################################################################
## FEATURE ENGINEERNIG ###
# using superml package for label encoding
#gonna label all the cat variables
#school
lb1 <- LabelEncoder$new()
std_df$school <-  lb1$fit_transform(std_df$school)
#sex
lb2 <- LabelEncoder$new()
std_df$sex <- lb2$fit_transform(std_df$sex)
#address
lb3 <- LabelEncoder$new()
std_df$address <- lb3$fit_transform(std_df$address)
#famsize
lb4 <-  LabelEncoder$new()
std_df$famsize <-  lb4$fit_transform(std_df$famsize)
#pstatus
lb5 <- LabelEncoder$new()
std_df$parent_cohabitation_status <- lb5$fit_transform(std_df$parent_cohabitation_status)
#Mjob
lb6 <- LabelEncoder$new()
std_df$Mjob <-  lb6$fit_transform(std_df$Mjob)
#Fjob
lb7 <- LabelEncoder$new()
std_df$Fjob <- lb7$fit_transform(std_df$Fjob)
#reason
lb8 <- LabelEncoder$new()
std_df$reason <-lb8$fit_transform(std_df$reason)
#guardian
lb9 <- LabelEncoder$new()
std_df$guardian <- lb9$fit_transform(std_df$guardian)
#schoolsup
lb10 <-  LabelEncoder$new()
std_df$schoolsup <-  lb10$fit_transform(std_df$schoolsup)
#famup
lb11 <- LabelEncoder$new()
std_df$famsup <-  lb11$fit_transform(std_df$famsup)
#paid
lb12 <-  LabelEncoder$new()
std_df$paid <- lb12$fit_transform(std_df$paid)
#activities
lb13 <-  LabelEncoder$new()
std_df$activities <- lb13$fit_transform(std_df$activities)
#nursery
lb14 <- LabelEncoder$new()
std_df$nursery <- lb14$fit_transform(std_df$nursery)
#higher
lb15 <-  LabelEncoder$new()
std_df$higher <- lb15$fit_transform(std_df$higher)
#internet
lb16 <- LabelEncoder$new()
std_df$internet <-  lb16$fit_transform(std_df$internet)
#romantic
lb17 <-  LabelEncoder$new()
std_df$romantic <- lb17$fit_transform(std_df$romantic)
write.csv(std_df, 'converted_df.csv')

#######################################################
std_df$Success_Status <- as.factor(std_df$Success_Status)
std_df$failures <- as.factor(std_df$failures)
std_df$age <- as.factor(std_df$age)
std_df$absences <- as.factor(std_df$absences)
std_df$studytime <- as.factor(std_df$studytime)
std_df$schoolsup <- as.factor(std_df$schoolsup)
std_df$famsup <- as.factor(std_df$famsup)


featurecols <- c('failures','age','absences',
                 'studytime','schoolsup','famsup',
                 'Success_Status')

std_filtered <- std_df[, featurecols]
head(std_filtered, 5)
# std_filtered$Success_Status <- as.integer(std_filtered$Success_Status)
str(std_filtered)
write.csv(std_filtered, 'filtered_df.csv')
###################
# rawcols <- c('school','age','address','famsize','parent_cohabitation_status',
#              'mother_edu','father_edu','Mjob','Fjob','reason','guardian',
#              'traveltime','studytime','failures','schoolsup',
#              'famsup','paid','activities','nursery','higher','internet','romantic',
#              'famrel','freetime','goout','Dalc','Walc','health','absences',
#              'Success_Status')
# std_raw <- std_df[, rawcols]
# head(std_raw,2)
#data spliting
library(caret)
set.seed(3456)

train_filt <- createDataPartition(std_filtered$Success_Status, p = 0.80, list = FALSE)
head(train_filt)
#splitiing the data
train_filt_data <- std_filtered[train_filt, ]
test_filt_data <- std_filtered[-train_filt, ]
str(train_filt_data)
str(test_filt_data)

#checking proportion
(nrow(train_filt_data) / (nrow(train_filt_data) + nrow(test_filt_data)))
(nrow(test_filt_data) / (nrow(train_filt_data) + nrow(test_filt_data)))

#chosing logistics regression to start with
logit_model_filt <- train(Success_Status~.,family = 'binomial', data = train_filt_data, method = 'glm')
summary(logit_model_filt)
#predicting
log_filt_pred <- predict(logit_model_filt, test_filt_data)
log_filt_CM <- confusionMatrix(log_filt_pred, test_filt_data[,"Success_Status"])
log_filt_CM

F_meas(test_filt_data$Success_Status, log_filt_pred ,relevant = levels(y_pred)[1])

misClassError <-  mean(log_filt_pred != test_filt_data$Success_Status)
print(paste('Accuracy =', 1-misClassError))

#
# fitcontrol <-  trainControl(method = 'repeatedcv', number = 10, repeats = 10)
# gbmFit1 <- train(Success_Status ~., data = train_filt_data, method = 'gbm', trControl = fitcontrol,
#                  verbose = FALSE)
# 
# plot(gbmFit1)
# plot(gbmFit1, metric = "Kappa")
# 
# gbmPrediction <- predict(gbmFit1, test_filt_data)


##############################################################################################
# Using Decision Tree#
########################################################
library(rpart) # recurssive partinioning
DecTreeModel <-  rpart(Success_Status ~., data = train_filt_data,method = 'class')
#see the dec tree
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(DecTreeModel)
summary(DecTreeModel)
printcp(DecTreeModel)

# step3: predict on test data
DecTree_pred <- predict(DecTreeModel, newdata = test_filt_data, type = 'class')
#step 4: evaluate the model accuracy
Dec_filt_CM <- confusionMatrix(DecTree_pred, test_filt_data[,"Success_Status"])
Dec_filt_CM


misClassError <-  mean(DecTree_pred != test_filt_data$Success_Status)
print(paste('Accuracy =', 1-misClassError))
# gbmConfMat <- confusionMatrix(gbmPrediction, test_filt_data[,"Success_Status"])
# gbmConfMat
F_meas(test_filt_data$Success_Status, DecTree_pred ,relevant = levels(y_pred)[1])
#####################################################################################
## Using Naive Bayes Classifier
######################################################
install.pkg('e1071')
library(e1071)
levels(train_filt_data$Success_Status)
model <- naiveBayes(Success_Status~., data = train_filt_data)
class(model)
pred <- predict(model, test_filt_data)
table(pred)
naBaye_CM <- confusionMatrix(pred, test_filt_data[,'Success_Status'])
naBaye_CM
#
naBayes_misClassError <-  mean(pred != test_filt_data$Success_Status)
print(paste('Accuracy =', 1-misClassError))

classifier_filt <-  naiveBayes(x = train_filt_data, y = train_filt_data$Success_Status)
# predict the test set results
y_pred = predict(classifier_filt, newdata = test_filt_data)


#makin confusion matrix
cm = table(test_filt_data$Success_Status, y_pred)
cm
F_meas(test_filt_data$Success_Status, y_pred ,relevant = levels(y_pred)[1], beta = 1)

misClassError <-  mean(y_pred != test_filt_data$Success_Status)
print(paste('Accuracy =', 1-misClassError))


