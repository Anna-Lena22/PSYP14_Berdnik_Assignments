#Assignment 2


#loading packages and custom functions:

library(tidyverse)
library(psych)
library(lm.beta)
library(gridExtra)
library(car)
library(boot)
library(lmboot)
library(sandwich)
library(data.table)


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

#loading dataset (home_sample_1):

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)

#for exploration of the dataset see Assignment 1

#"removing" outlier value (age in ID93) like done in Assignment 1:
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(data_sample_1, "age", which (data_sample_1$age > 100), NA)

view(data_sample_1)



#Research question 2: Backward Regression + comparison
#creating new model with all variables (excluding cortisol_saliva):
initial_model = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)


#model diagnostics:

#significant outliers? (only for new variables in model)
data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = weight, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = IQ, y = pain, label = rownum) + geom_point() +
  geom_label()

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = household_income, y = pain, label = rownum) + geom_point() +
  geom_label()


data_sample_1 %>% ggplot() + aes(x = weight, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_sample_1 %>% ggplot() + aes(x = IQ, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_sample_1 %>% ggplot() + aes(x = household_income, y = pain) + geom_point() +
  geom_smooth(method = "lm")


initial_model %>% plot(which = 5)
initial_model %>% plot(which = 4)

data_sample_1 %>% slice(c(3, 103, 114))
#variables will not be excluded, because of valid data


#normality?
initial_model %>% plot(which = 2)

residuals_in_mod = enframe(residuals(initial_model))
residuals_in_mod %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(initial_model))
#skewness and kurtosis are not harmed


#Linearity?
initial_model %>% residualPlots()

#Homoscedasticity? 
initial_model %>% plot(which = 3)
initial_model %>% ncvTest()

#Multicollinearity?
initial_model %>% vif()




#backward regression with initial_model on datafile 1:
model_back = step(initial_model, direction = "backward")


#new regression model ("backward model") - only contains retained predictors:
backward_model = lm(pain ~ sex + household_income + mindfulness + age + pain_cat + cortisol_serum, data = data_sample_1)
summary(backward_model)

coef_table(backward_model)

#model comparison: initial backward model vs. final "backward model"
summary(initial_model)$adj.r.squared
summary(backward_model)$adj.r.squared

AIC(initial_model)
AIC(backward_model)

#regression model from Assignment 1 - saved in new object ("theory-based model"):
theorybased_model = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
summary(theorybased_model)


#model comparison: "backward model" vs. "theory-based model")
summary(backward_model)$adj.r.squared
summary(theorybased_model)$adj.r.squared

AIC(backward_model)
AIC(theorybased_model)
#no anova-function used here!


#testing the two models on new dataset:

#loading new dataset:
data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

#Predictions on new dataset: "backward model" vs. "theory-based model"
predict_backward = predict(backward_model, newdata = data_sample_2)
predict_backward

predict_theory = predict(theorybased_model, newdata = data_sample_2)
predict_theory


#prediction performance:
RSS_back = sum((data_sample_2$pain - predict(backward_model))^2)
RSS_back

RSS_theory = sum((data_sample_2$pain - predict(theorybased_model))^2)
RSS_theory

RSS_back - RSS_theory





