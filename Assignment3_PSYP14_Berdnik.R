#Assignment 3


#loading packages:
library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)
library(data.table)

#loading custom functions:

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}


#loading Datasets:

data_3 = read.csv("https://tinyurl.com/ha-dataset3")

data_4 = read.csv("https://tinyurl.com/ha-dataset4")

#checking data:
view(data_3)
view(data_4)

describe(data_3)
describe(data_4)

#visualization data_3:
data_3 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()

data_3 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_point()
#error: female wrongly spelled

data_3 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_3 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

data_3 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

data_3 %>% 
  ggplot()+
  aes(x = cortisol_serum, y = pain)+
  geom_point()

data_3 %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()
#error: values under zero 



data_3 %>% 
  ggplot()+
  aes(x = hospital, y = pain)+
  geom_point()



#visualization data_4:

data_4 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = cortisol_serum, y = pain)+
  geom_point()

data_4 %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()
#error: values under zero 

data_4 %>% 
  ggplot()+
  aes(x = hospital, y = pain)+
  geom_point()




#finding errors in data and correcting them:

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(data_3, "household_income", which (data_3$household_income < 0), NA) #replace errors with "NA"

data_3 = data_3 %>% slice(-c(182)) #removing the participant with the spelling mistake

view(data_3)

describe(data_3)

outlierReplace(data_4, "household_income", which (data_4$household_income < 0), NA) #replace errors with "NA"

describe(data_4)




#asign hospital as a grouping factor

data_3 %>% 
  mutate(hospital = factor(hospital))

#exploring clustering in data

data_3 %>% 
  ggplot() +
  aes(y = pain, x = sex) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)


data_3 %>% 
  ggplot() +
  aes(y = pain, x = age) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)

data_3 %>% 
  ggplot() +
  aes(y = pain, x = STAI_trait) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)

data_3 %>% 
  ggplot() +
  aes(y = pain, x = pain_cat) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)

data_3 %>% 
  ggplot() +
  aes(y = pain, x = mindfulness) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)

data_3 %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)




#building a linear mixed model on data_3 - clustering for hospital sites
#random intercept model (random intercept = hospital)

mod_inter = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_3)


#get coefficients and CI for the intercept model
summary(mod_inter)
confint(mod_inter)

stdCoef.merMod(mod_inter)


#variance explained by fixed effects (marginal R^2)
r2beta(mod_inter, method = "nsj", data = data_3)

#variance explained by fixed and random effects (conditional R^2)
r.squaredGLMM(mod_inter) 

#predicting pain in data_4 with regression equation
predict_pain <- predict(mod_inter, data_4, allow.new.levels = TRUE)
predict_pain

#computing variance explained by model on data 4:
RSS = sum((data_4$pain - predict_pain)^2)
RSS

mod_mean <- lm(pain ~ 1, data = data_4) 

TSS = sum((data_4$pain - predict(mod_mean))^2)
TSS

R2 = 1-(RSS/TSS)
R2

#building new linear mixed effect model on data 3 (only influencial predictors included):
summary(mod_inter)

new_mixmodel = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_3)

#vizualizing regression lines for each hospital separately:
windows()
slope_plot = data_3 %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

slope_plot





























































