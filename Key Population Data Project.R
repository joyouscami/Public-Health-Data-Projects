options(prompt="R>")
library(readxl)
library(dplyr)
library(car)
library(broom)
getwd()
data<-read_excel("C:/R Programming/Atlo Research Data.xlsx")
str(data)
data$satisfaction_rate_encoded<-as.numeric(factor(data$satisfaction_rate))
#Performing a T-Test to compare satisfaction rate between two groups
enrolled<-data %>% filter(hiv_enrollment==
                            "i_am_currently_enrolled_in_the_hiv_progr") %>% pull(
                              satisfaction_rate_encoded)
not_enrolled<-data %>% filter(hiv_enrollment != 
                                "i_am_currently_enrolled_in_the_hiv_progr") %>% pull(
                                  satisfaction_rate_encoded)
t_test_result<-t.test(enrolled,not_enrolled, na.rm = TRUE)
print(t_test_result)
#Performing ANOVA to compare means of satisfaction Rates across sub counties
anova_model<-aov(satisfaction_rate_encoded ~ Sub_County, data=data)
anova_result<-Anova(anova_model,type="II")
summary(anova_result)
#Logistic Regression to predict likelihood of HIV Service uptake
#Encoding the HIV Enrollment as binary
data$hiv_enrollment_encoded<-ifelse(data$hiv_enrollment==
                                      "i_am_currently_enrolled_in_the_hiv_progr",1,0)
logit_model<-glm(hiv_enrollment_encoded~Age_of_Interviewee,data=data,family=binomial)
summary(logit_model)
tidy(logit_model)
#Factor analysis to determine the underlying factors
library(psych)
library(GPArotation)
#converting factors into numeric
data$Age_of_Interviewee<-as.numeric(as.factor(data$Age_of_Interviewee))
data$Sub_County<-as.factor(data$Sub_County)
data$Sub_County_numeric<-as.numeric(data$Sub_County)
levels(data$Sub_County)
data$hiv_enrollment<-as.numeric(as.factor(data$hiv_enrollment))
data$satisfaction_rate<-as.numeric(as.factor(data$satisfaction_rate))
#determining number of factors
data<-data.frame(lapply(data,function(x)if(is.factor(x)||is.character(x))
  as.numeric(as.factor(x))else x))
fa.parallel(data,fa="fa")

