data<-read.csv("smoking_dataset.csv")
str(data)

data$gender<- as.factor(data$gender)
data$hearing.left.<- as.factor(data$hearing.left.)
data$hearing.right.<-as.factor(data$hearing.right.)
data$Urine.protein<-as.factor(data$Urine.protein)
data$oral<-as.factor(data$oral)
data$tartar<-as.factor(data$tartar)
data$dental.caries<-as.factor(data$dental.caries)
data$smoking<-as.factor(data$smoking)

str(data)

q<-na.omit(data)

library(ggplot2)

ggplot(data,aes(y=age))+geom_boxplot()
IQR_age<-IQR(data$age)
IQR_age
lower_bound<-quantile(data$age,0.25)-1.5*IQR_age
upper_bound<-quantile(data$age,0.25)+1.5*IQR_age
y<-data[data$age>=lower_bound&data$age<=upper_bound,]
boxplot(y$age)

ggplot(data,aes(y=height.cm.))+geom_boxplot()
IQR_height.cm.<-IQR(data$height.cm.)
IQR_height.cm.
lower_bound<-quantile(data$height.cm.,0.25)-1.5*IQR_height.cm.
upper_bound<-quantile(data$height.cm.,0.25)+1.5*IQR_height.cm.
y<-data[data$height.cm.>=lower_bound&data$height.cm.<=upper_bound,]
boxplot(y$height.cm.)

ggplot(data,aes(y=weight.kg.))+geom_boxplot()
IQR_weight.kg.<-IQR(data$weight.kg.)
IQR_weight.kg.
lower_bound<-quantile(data$weight.kg.,0.25)-1.5*IQR_weight.kg.
upper_bound<-quantile(data$weight.kg.,0.25)+1.5*IQR_weight.kg.
y<-data[data$weight.kg.>=lower_bound&data$weight.kg.<=upper_bound,]
boxplot(y$weight.kg.)

ggplot(data,aes(y=waist.cm.))+geom_boxplot()
IQR_waist.cm.<-IQR(data$waist.cm.)
IQR_waist.cm.
lower_bound<-quantile(data$waist.cm.,0.25)-1.5*IQR_waist.cm.
upper_bound<-quantile(data$waist.cm.,0.25)+1.5*IQR_waist.cm.
y<-data[data$waist.cm.>=lower_bound&data$waist.cm.<=upper_bound,]
boxplot(y$waist.cm.)

ggplot(data,aes(y=eyesight.left.))+geom_boxplot()
IQR_eyesight.left.<-IQR(data$eyesight.left.)
IQR_eyesight.left.
lower_bound<-quantile(data$eyesight.left.,0.25)-1.5*IQR_eyesight.left.
upper_bound<-quantile(data$eyesight.left.,0.25)+1.5*IQR_eyesight.left.
y<-data[data$eyesight.left.>=lower_bound&data$eyesight.left.<=upper_bound,]
boxplot(y$eyesight.left.)

ggplot(data,aes(y=eyesight.right.))+geom_boxplot()
IQR_eyesight.right.<-IQR(data$eyesight.right.)
IQR_eyesight.right.
lower_bound<-quantile(data$eyesight.right.,0.25)-1.5*IQR_eyesight.right.
upper_bound<-quantile(data$eyesight.right.,0.25)+1.5*IQR_eyesight.right.
y<-data[data$eyesight.right.>=lower_bound&data$eyesight.right.<=upper_bound,]
boxplot(y$eyesight.right.)

ggplot(data,aes(y=systolic))+geom_boxplot()
IQR_systolic<-IQR(data$systolic)
IQR_systolic
lower_bound<-quantile(data$systolic,0.25)-1.5*IQR_systolic
upper_bound<-quantile(data$systolic,0.25)+1.5*IQR_systolic
y<-data[data$systolic>=lower_bound&data$systolic<=upper_bound,]
boxplot(y$systolic)

ggplot(data,aes(y=relaxation))+geom_boxplot()
IQR_relaxation<-IQR(data$systolic)
IQR_relaxation
lower_bound<-quantile(data$relaxation,0.25)-1.5*IQR_relaxation
upper_bound<-quantile(data$relaxation,0.25)+1.5*IQR_relaxation
y<-data[data$relaxation>=lower_bound&data$relaxation<=upper_bound,]
boxplot(y$relaxation)

ggplot(data,aes(y=fasting.blood.sugar))+geom_boxplot()
IQR_fasting.blood.sugar<-IQR(data$fasting.blood.sugar)
IQR_fasting.blood.sugar
lower_bound<-quantile(data$fasting.blood.sugar,0.25)-1.5*IQR_fasting.blood.sugar
upper_bound<-quantile(data$fasting.blood.sugar,0.25)+1.5*IQR_fasting.blood.sugar
y<-data[data$fasting.blood.sugar>=lower_bound&data$fasting.blood.sugar<=upper_bound,]
boxplot(y$fasting.blood.sugar)

ggplot(data,aes(y=Cholesterol))+geom_boxplot()
IQR_Cholesterol<-IQR(data$Cholesterol)
IQR_Cholesterol
lower_bound<-quantile(data$Cholesterol,0.25)-1.5*IQR_Cholesterol
upper_bound<-quantile(data$Cholesterol,0.25)+1.5*IQR_Cholesterol
y<-data[data$Cholesterol>=lower_bound&data$Cholesterol<=upper_bound,]
boxplot(y$Cholesterol)

ggplot(data,aes(y=triglyceride))+geom_boxplot()
IQR_triglyceride<-IQR(data$triglyceride)
IQR_triglyceride
lower_bound<-quantile(data$triglyceride,0.25)-1.5*IQR_triglyceride
upper_bound<-quantile(data$triglyceride,0.25)+1.5*IQR_triglyceride
y<-data[data$triglyceride>=lower_bound&data$triglyceride<=upper_bound,]
boxplot(y$triglyceride)

ggplot(data,aes(y=HDL))+geom_boxplot()
IQR_HDL<-IQR(data$HDL)
IQR_HDL
lower_bound<-quantile(data$HDL,0.25)-1.5*IQR_HDL
upper_bound<-quantile(data$HDL,0.25)+1.5*IQR_HDL
y<-data[data$HDL>=lower_bound&data$HDL<=upper_bound,]
boxplot(y$HDL)

ggplot(data,aes(y=LDL))+geom_boxplot()
IQR_LDL<-IQR(data$LDL)
IQR_LDL
lower_bound<-quantile(data$LDL,0.25)-1.5*IQR_LDL
upper_bound<-quantile(data$LDL,0.25)+1.5*IQR_LDL
y<-data[data$LDL>=lower_bound&data$LDL<=upper_bound,]
boxplot(y$LDL)

ggplot(data,aes(y=hemoglobin))+geom_boxplot()
IQR_hemoglobin<-IQR(data$hemoglobin)
IQR_hemoglobin
lower_bound<-quantile(data$hemoglobin,0.25)-1.5*IQR_hemoglobin
upper_bound<-quantile(data$hemoglobin,0.25)+1.5*IQR_hemoglobin
y<-data[data$hemoglobin>=lower_bound&data$hemoglobin<=upper_bound,]
boxplot(y$hemoglobin)

ggplot(data,aes(y=serum.creatinine))+geom_boxplot()
IQR_serum.creatinine<-IQR(data$serum.creatinine)
IQR_serum.creatinine
lower_bound<-quantile(data$serum.creatinine,0.25)-1.5*IQR_serum.creatinine
upper_bound<-quantile(data$serum.creatinine,0.25)+1.5*IQR_serum.creatinine
y<-data[data$serum.creatinine>=lower_bound&data$serum.creatinine<=upper_bound,]
boxplot(y$serum.creatinine)

ggplot(data,aes(y=AST))+geom_boxplot()
IQR_AST<-IQR(data$AST)
IQR_AST
lower_bound<-quantile(data$AST,0.25)-1.5*IQR_AST
upper_bound<-quantile(data$AST,0.25)+1.5*IQR_AST
y<-data[data$AST>=lower_bound&data$AST<=upper_bound,]
boxplot(y$AST)

ggplot(data,aes(y=ALT))+geom_boxplot()
IQR_ALT<-IQR(data$ALT)
IQR_ALT
lower_bound<-quantile(data$ALT,0.25)-1.5*IQR_ALT
upper_bound<-quantile(data$ALT,0.25)+1.5*IQR_ALT
y<-data[data$ALT>=lower_bound&data$ALT<=upper_bound,]
boxplot(y$ALT)

ggplot(data,aes(y=Gtp))+geom_boxplot()
IQR_Gtp<-IQR(data$Gtp)
IQR_Gtp
lower_bound<-quantile(data$Gtp,0.25)-1.5*IQR_Gtp
upper_bound<-quantile(data$Gtp,0.25)+1.5*IQR_Gtp
y<-data[data$Gtp>=lower_bound&data$Gtp<=upper_bound,]
boxplot(y$Gtp)

str(y)

mean(y$age)
mean(y$height.cm.)
mean(y$weight.kg.)
mean(y$waist.cm.)
mean(y$eyesight.left.)
mean(y$eyesight.right.)
mean(y$systolic)
mean(y$relaxation)
mean(y$fasting.blood.sugar)
mean(y$Cholesterol)
mean(y$triglyceride)
mean(y$HDL)
mean(y$LDL)
mean(y$hemoglobin)
mean(y$serum.creatinine)
mean(y$AST)
mean(y$ALT)
mean(y$Gtp)

median(y$age)
median(y$height.cm.)
median(y$weight.kg.)
median(y$waist.cm.)
median(y$eyesight.left.)
median(y$eyesight.right.)
median(y$systolic)
median(y$relaxation)
median(y$fasting.blood.sugar)
median(y$Cholesterol)
median(y$triglyceride)
median(y$HDL)
median(y$LDL)
median(y$hemoglobin)
median(y$serum.creatinine)
median(y$AST)
median(y$ALT)
median(y$Gtp)

sd(y$age)
sd(y$height.cm.)
sd(y$weight.kg.)
sd(y$waist.cm.)
sd(y$eyesight.left.)
sd(y$eyesight.right.)
sd(y$systolic)
sd(y$relaxation)
sd(y$fasting.blood.sugar)
sd(y$Cholesterol)
sd(y$triglyceride)
sd(y$HDL)
sd(y$LDL)
sd(y$hemoglobin)
sd(y$serum.creatinine)
sd(y$AST)
sd(y$ALT)
sd(y$Gtp)

var(y$age)
var(y$height.cm.)
var(y$weight.kg.)
var(y$waist.cm.)
var(y$eyesight.left.)
var(y$eyesight.right.)
var(y$systolic)
var(y$relaxation)
var(y$fasting.blood.sugar)
var(y$Cholesterol)
var(y$triglyceride)
var(y$HDL)
var(y$LDL)
var(y$hemoglobin)
var(y$serum.creatinine)
var(y$AST)
var(y$ALT)
var(y$Gtp)


str(y)
library(ggplot2)

ggplot(y,aes(x=age))+geom_histogram()
ggplot(y,aes(x=height.cm.))+geom_histogram()
ggplot(y,aes(x=weight.kg.))+geom_histogram()
ggplot(y,aes(x=waist.cm.))+geom_histogram()
ggplot(y,aes(x=eyesight.left.))+geom_histogram()
ggplot(y,aes(x=eyesight.right.))+geom_histogram()
ggplot(y,aes(x=systolic))+geom_histogram()
ggplot(y,aes(x=relaxation))+geom_histogram()
ggplot(y,aes(x=fasting.blood.sugar))+geom_histogram()
ggplot(y,aes(x=HDL))+geom_histogram()
ggplot(y,aes(x=LDL))+geom_histogram()
ggplot(y,aes(x=hemoglobin))+geom_histogram()
ggplot(y,aes(x=Cholesterol))+geom_histogram()
ggplot(y,aes(x=triglyceride))+geom_histogram()
ggplot(y,aes(x=serum.creatinine))+geom_histogram()
ggplot(y,aes(x=AST))+geom_histogram()
ggplot(y,aes(x=ALT))+geom_histogram()
ggplot(y,aes(x=Gtp))+geom_histogram()

library(ggplot2) 
ggplot(y,aes(x=gender))+geom_bar()
ggplot(y,aes(x=hearing.left.))+geom_bar()
ggplot(y,aes(x=hearing.right.))+geom_bar()
ggplot(y,aes(x=Urine.protein))+geom_bar()
ggplot(y,aes(x=dental.caries))+geom_bar()
ggplot(y,aes(x=tartar))+geom_bar()
ggplot(y,aes(x=smoking))+geom_bar()

abc<-table(y$smoking,y$gender)
abc
barplot(abc,legend.text = TRUE)

abc<-table(y$smoking,y$hearing.left.)
abc
barplot(abc,legend.text = TRUE)

abc<-table(y$smoking,y$hearing.right.)
abc
barplot(abc,legend.text = TRUE)

abc<-table(y$smoking,y$Urine.protein)
abc
barplot(abc,legend.text = TRUE)

abc<-table(y$smoking,y$dental.caries)
abc
barplot(abc,legend.text = TRUE)

abc<-table(y$smoking,y$tartar)
abc
barplot(abc,legend.text = TRUE)



ggplot(y, aes(x =smoking , y = age)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = height.cm.)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = weight.kg.)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = waist.cm.)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = eyesight.left.)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = eyesight.right.)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = systolic)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = relaxation)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = fasting.blood.sugar)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = Cholesterol)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = triglyceride)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = HDL)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = LDL)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = hemoglobin)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = serum.creatinine)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = AST)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = ALT)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()
ggplot(y, aes(x =smoking , y = Gtp)) + geom_boxplot(fill = 'purple', color ='black', alpha = 0.7) + theme_minimal()


logistic_model <- glm(smoking ~ height.cm. + weight.kg. + triglyceride + hemoglobin + ALT + Gtp + tartar + gender + dental.caries, data=y, family = "binomial")
summary(logistic_model)


predictions <- predict(logistic_model, newdata = y, type = "response") 
predicted_classes <- ifelse(predictions > 0.5, 1, 0) 
conf_matrix <- table(predicted_classes, y$smoking)
table(predicted_classes, y$smoking)


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sum(diag(conf_matrix)) / sum(conf_matrix)


install.packages("randomForest")
library(randomForest)
set.seed(123) 
sample_index <- sample(1:nrow(y), 0.7 * nrow(y))
train_data <- y[sample_index, ]
test_data <- y[-sample_index, ] 

rf_model <- randomForest(smoking ~ ., data = train_data, ntree = 100)

predictions <- predict(rf_model, newdata = test_data)

accuracy <- sum(predictions == test_data$smoking) / nrow(test_data)
print(paste("Accuracy:", accuracy))

