options(prompt="R>")
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
setwd("C:/R Programming")
#Loading the data
data <- read.csv("C:/R Programming/Maternal Health Risk.csv")
str(data)
#Checking for missing values
sum(is.na(data))
#Data exploration
summary(data)
#histograms for each attribute
ggplot(data,aes(x=Age))+geom_histogram(binwidth=2)+ggtitle("Age Distribution")
ggplot(data,aes(x=SystolicBP))+geom_histogram(binwidth=5)+
  ggtitle("Systolic BP Distribution")
ggplot(data,aes(x=DiastolicBP))+geom_histogram(binwidth=5)+
  ggtitle("Diastolic BP Distribution")
ggplot(data,aes(x=BS))+geom_histogram(binwidth=0.5)+
  ggtitle("Blood Glucose Levels Distribution",subtitle= "Blood sugar levels")
ggplot(data,aes(x=HeartRate))+geom_histogram(binwidth=5)+
  ggtitle("Heart Rate Distribution", subtitle = "How is your heart beating?")
#correlation matrix
cor_matrix<-cor(data %>%select(-RiskLevel))
print(cor_matrix)
library(corrplot)
corrplot(cor_matrix,method="circle")
#multinomial logistic regression 
library(nnet)
data$RiskLevel<-as.factor(data$RiskLevel)
model_multinom <- multinom(RiskLevel ~ Age + SystolicBP + DiastolicBP + BS + 
                             HeartRate,data=data)
summary(model_multinom)
risk_frequencies<-table(data$RiskLevel)
print(risk_frequencies)
barplot(risk_frequencies,main="Frequencies of Risk Levels",xlab="Risk Level",
        ylab="Frequency",col="blue")
data$RiskLevel<-relevel(data$RiskLevel,ref="low risk") 
model_multinom <- multinom(RiskLevel ~ Age + SystolicBP + DiastolicBP + BS +
                             HeartRate,data=data)
summary(model_multinom)
data$RiskLevel<-relevel(data$RiskLevel,ref="mid risk")
model_multinom <- multinom(RiskLevel ~ Age + SystolicBP + DiastolicBP + BS + 
                             HeartRate,data=data)
summary(model_multinom)
#Assessing risk level across age groups
age_breaks<-c(0,20,30,40,50,100)
age_labels<-c("0-20","21-30","31-40","41-50","51+")
data$AgeGroup<-cut(data$Age,breaks=age_breaks,labels=age_labels,right=FALSE)
risk_age_table<-table(data$AgeGroup,data$RiskLevel)
print(risk_age_table)
risk_age_prop<-prop.table(risk_age_table,margin=1)
print(risk_age_prop)
#visualizing the results
ggplot(data,aes(x=AgeGroup,fill=RiskLevel))+geom_bar(position="Fill")+
  labs(title="Distribution of Risk Levels aross Age Groups",
       X="AgeGroup",y="proportion")+theme_minimal()
