#Assignment 1


#Loading Packages:

library(tidyverse)
library(psych)
library(lm.beta)
library(gridExtra)
library(car)
library(boot)
library(lmboot)
library(sandwich)
library(data.table)

#Loading custom functions:

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

bs_to_boot <- function(model, data, indices) {
  d <- data[indices, ] # allows boot to select sample
  fit <- lm(formula(model), data = d)
  return(coef(fit))
}

adjR2_to_boot <- function(model, data, indices) {
  d <- data[indices, ] # allows boot to select sample
  fit <- lm(formula(model), data = d)
  return(summary(fit)$adj.r.squared)
}

confint.boot <- function(model, data = NULL, R = 1000) {
  if (is.null(data)) {
    data = eval(parse(text = as.character(model$call[3])))
  }
  2
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)),
                                              ncol = 2))
  row.names(boot.ci_output_table) = names(coef(model))
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")
  results.boot = results <- boot(data = data, statistic = bs_to_boot,
                                 R = 1000, model = model)
  for (i in 1:length(coef(model))) {
    boot.ci_output_table[i, ] = unlist(unlist(boot.ci(results.boot,
                                                      type = "bca", index = i))[c("bca4", "bca5")])
  }
  return(boot.ci_output_table)
}

wild.boot.confint <- function(model, data = NULL, B = 1000) {
  if (is.null(data)) {
    data = eval(parse(text = as.character(model$call[3])))
  }
  wild_boot_estimates = wild.boot(formula(model), data = data,
                                  B = B)
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,
                                                                     probs = c(0.025, 0.975))))
  return(result)
}  
  


#Loading dataset:

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")



#Checking Dataset:

data_sample_1 %>% summary()

describe(data_sample_1)

view(data_sample_1) 



#histograms:
data_sample_1 %>% 
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = age) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50)

data_sample_1 %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram(bins = 50)


#scatterplot
data_sample_1 %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = sex, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_serum, y = pain)+
  geom_point()

data_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_saliva, y = pain)+
  geom_point()




#Research question 1: Hierarchical Regression (model1, model2)
#creating models

model1 <- lm(pain ~ sex + age, data = data_sample_1)

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)



#model diagnostics - model1:

#extreme outliers? (sex, age)

data_sample_1 %>% 
  ggplot() + 
  aes(x = sex, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

data_sample_1 %>% 
  ggplot() + 
  aes(x = age, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

#cook's distance:

model1 %>% plot(which = 4)

data_sample_1 %>% slice(c(93))

#"removing" outlier (age in ID93) because of an error in data. Influences x-axis and statistical parameters! Assumptions are harmed!
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(data_sample_1, "age", which (data_sample_1$age > 100), NA)

view(data_sample_1)

#recode model 1 with dataset without outlier:
model1 <- lm(pain ~ sex + age, data = data_sample_1)


#repeating + moving on with model diagnostics (but corrected for outlier)

#outliers?
data_sample_1 %>% 
  ggplot() + 
  aes(x = sex, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

data_sample_1 %>% 
  ggplot() + 
  aes(x = age, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

model1 %>% plot(which = 4)

#normality?
model1 %>% plot(which = 2)

residuals_model1 = enframe(residuals(model1))
residuals_model1 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model1))

#linearity?
model1 %>% residualPlots()


#homoscedasticity?
model1 %>% plot(which = 3)
model1 %>% ncvTest()

#multicollinearity?
model1 %>% vif()




#model diagnostics model2 (using outlier correction from before for age):

#extreme outliers? (checking on additional variables: STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva)
data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = STAI_trait, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = pain_cat, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = cortisol_serum, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = cortisol_saliva, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = mindfulness, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% 
  ggplot() +
  aes(x = age, y = pain, label = rownum) + geom_point() +
  geom_label()


#running model2 again to "remove outlier (age in ID93))
model2 = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)

#cook's distance:
model2 %>% plot(which = 4)

data_sample_1 %>% slice(c(68, 114, 150))
#outliers are not harming assumptions of regression(e.g. normality)! They are no errors and valid data!

#normality?
model2 %>% plot(which = 2)

residuals_model2 = enframe(residuals(model2))
residuals_model2 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

#linearity?
model2 %>% residualPlots()

#homoscedasticity?
model2 %>% plot(which = 3)
model2 %>% ncvTest()

#multicollinearity?
model2 %>% vif()

data_sample_1 %>% 
  select(pain, sex, age, STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum) %>% 
  pairs.panels(col = "red", lm = T)

#removing corticol_saliva, because it highly correlates with cortisol_serum (cortisol_serum is more reliable according to theory):
model2_no_multi = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

#running model diagnostics ones again with new mode: model2_no_multi
#extreme outliers? - cook's distance:

model2_no_multi %>% plot(which = 4)

#normality?
model2_no_multi %>% plot(which = 2)

residuals_model2_nomulti = enframe(residuals(model2_no_multi))
residuals_model2_nomulti %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2_no_multi))

#linearity?
model2_c_no_multi %>% residualPlots()

#homoscedasticity?
model2_no_multi %>% plot(which = 3)
model2_no_multi %>% ncvTest()

#multicollinearity?
model2_no_multi %>% vif()



#model comparison + final results:

summary(model1)
summary(model2_no_multi)

summary(model1)$adj.r.squared
summary(model2_no_multi)$adj.r.squared

AIC(model1)

AIC(model2_no_multi)

anova(model1, model2_no_multi) 


confint(model2_no_multi)

lm.beta(model2_no_multi)

coef_table(model1)
coef_table(model2_no_multi)

