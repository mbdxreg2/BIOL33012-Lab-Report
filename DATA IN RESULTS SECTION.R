library(tidyverse)
library(ggplot2)
library(afex)
library(emmeans)
library(Hmisc)
library(rcompanion)

#Uploading data file and separating it into SST and DRT tasks

my_data <- read_csv("My_Data.csv")

my_data_stop <- filter(my_data, Motor_Condition == "Stop")
str(my_data_stop)

my_data_continue <- filter(my_data, Motor_Condition == "Continue")
str(my_data_continue)

#Descriptive Stats for Motor Tasks
my_data_stop %>%
  summarise(Mean = mean(Motor_Normal), SD = sd(Motor_Normal))

groupwiseMean(Motor_Normal ~ 1,
              data   = my_data_stop,
              conf   = 0.95,
              digits = 4)

my_data_stop %>%
  summarise(Mean = mean(Motor_Red), SD = sd(Motor_Red))

groupwiseMean(Motor_Red ~ 1,
              data   = my_data_stop,
              conf   = 0.95,
              digits = 4)

my_data_stop %>%
  summarise(Mean = mean(Motor_Task_RT), SD = sd(Motor_Task_RT))

groupwiseMean(Motor_Task_RT ~ 1,
              data   = my_data_stop,
              conf   = 0.95,
              digits = 4)

my_data_continue %>%
  summarise(Mean = mean(Motor_Normal), SD = sd(Motor_Normal))

groupwiseMean(Motor_Normal ~ 1,
              data   = my_data_continue,
              conf   = 0.95,
              digits = 4)

my_data_continue %>%
  summarise(Mean = mean(Motor_Red), SD = sd(Motor_Red))

groupwiseMean(Motor_Red ~ 1,
              data   = my_data_continue,
              conf   = 0.95,
              digits = 4)

my_data_continue %>%
  summarise(Mean = mean(Motor_Task_RT), SD = sd(Motor_Task_RT))

groupwiseMean(Motor_Task_RT ~ 1,
              data   = my_data_continue,
              conf   = 0.95,
              digits = 4)

#Linear Regression Model for MSN

rcorr(my_data$Motor_Score, my_data$Mean_Stake_Number)

model0 <- lm (Mean_Stake_Number ~ 1, data = my_data)
model1 <- lm (Mean_Stake_Number ~ Motor_Score, data = my_data)

anova(model0, model1)
#significant

summary(model1)

#Linear Regression Model for MSR

rcorr(my_data$Attentional_Score, my_data$Mean_Stake_Risk)

model2 <- lm (Mean_Stake_Risk ~ 1, data = my_data)
model3 <- lm (Mean_Stake_Risk ~ Attentional_Score, data = my_data)

anova(model2, model3)
#Significant

summary(model3)

#ANOVA for mean GSR

model_GSR_mean <- aov_4(GSR_Mean ~ Motor_Condition * BIS_Type + (1 | ID),
                        data = my_data)

anova(model_GSR_mean)

emmeans(model_GSR_mean, pairwise ~ Motor_Condition * BIS_Type, adjust = "none")