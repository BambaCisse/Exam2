# Exam2
---
title: "Exam2"
author: "Bamba"
date: "2025-11-20"
output: html_document
   storyboard: true
   social: menu
   source: embed
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

---

```{r setup, include=FALSE}

# Load required libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(knitr)
library(dplyr)

# Set chunk options
knitr::opts_chunk$set(echo = TRUE)


# Load datasets
load("d_HHP2020_24.Rdata")
ls()


Question 2

dat <- d_HHP2020_24   # rename for convenience

ols_model1 <- lm(
  K4SUM ~ Education + income_midpoint + Age + Gender,  
  data = dat
)

summary(ols_model1)

ols_no_educ <- update(ols_model1, . ~ . - Education)

anova(ols_no_educ, ols_model1)

ols_no_income <- update(ols_model1, . ~ . - Income)

anova(ols_no_income, ols_model1)



#Question 3

sub <- dat %>%
  filter(
    Age >= 25, Age <= 64,
    !is.na(K4SUM),
    !is.na(Education),
    !is.na(income_midpoint)
  )

# Overall summary
sub %>%
  summarise(
    n         = n(),
    mean_K4   = mean(K4SUM),
    sd_K4     = sd(K4SUM),
    mean_age  = mean(Age),
    sd_age    = sd(Age)
  )


# Subset By income
sub %>%
  group_by(income_midpoint) %>%
  summarise(
    n       = n(),
    mean_K4 = mean(K4SUM),
    .groups = "drop"
  )

summary(sub %>%
  group_by(income_midpoint) %>%
  summarise(
    n       = n(),
    mean_K4 = mean(K4SUM),
    .groups = "drop"
  ))



##Create MentalHealth_01
sub <- sub %>%
  mutate(MentalHealth_01 = as.integer(K4SUM > 8))

#Question 4

4(a) Choose predictors + interactions

Example:

Education
Income
Age and Age²
Gender
Interaction: Education × Gender (ask if education works differently for women).

sub <- sub %>%
  mutate(
    female = as.integer(Gender == "Female"),
    Age2   = Age^2
  )

lpm <- lm(
  MentalHealth_01 ~ Education + income_midpoint + Age + Age2 + female +
                    Education:female,
  data = sub
)

summary(lpm)


##4C

lpm_no_educ <- update(lpm, . ~ . - Education)
anova(lpm_no_educ, lpm)


##4d

              predicted
# Predicted probabilities
pred_prob <- predict(lpm, type = "response")

# Convert to predicted class using cutoff 0.5
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

# Confusion matrix
tab <- table(
  truth = sub$MentalHealth_01,
  predicted = pred_class
)
tab

##Question 5

logit_mod <- glm(
  MentalHealth_01 ~ Education + income_midpoint + Age + Age2 + female +
                    Education:female,
  data = sub,
  family = binomial(link = "logit")
)

summary(logit_mod)

exp(coef(logit_mod))


logit_no_income <- update(logit_mod, . ~ . - Income)

anova(logit_no_income, logit_mod, test = "Chisq")

pred_prob_logit <- predict(logit_mod, newdata = new_people, type = "response")
pred_prob_logit



# 1. Refit the logit model on the same 'sub' you use for MentalHealth_01
logit_mod <- glm(
  MentalHealth_01 ~ Education + income_midpoint + Age + Age2 + Gender,
  data = sub,
  family = binomial(link = "logit")
)

# 2. Predicted probabilities (same n as nrow(sub))
prob_logit <- predict(logit_mod, type = "response")

# 3. Predicted classes using cutoff 0.5
class_logit <- ifelse(prob_logit >= 0.5, 1L, 0L)

# 4. Confusion matrix: now both vectors have the same length
tab_logit <- table(
  truth     = sub$MentalHealth_01,
  predicted = class_logit
)

tab_logit

Question#6
probit_mod <- glm(
  MentalHealth_01 ~ Education + income_midpoint + Age + Age2 + female +
                    Education:female,
  data = sub,
  family = binomial(link = "probit")
)

summary(probit_mod)

prob_probit  <- predict(probit_mod, type = "response")
class_probit <- ifelse(prob_probit >= 0.5, 1, 0)

tab_probit <- table(
  truth = sub$MentalHealth_01,
  pred  = class_probit
)
tab_probit

type1_rate_probit <- tab_probit["0","1"] / sum(sub$MentalHealth_01 == 0)
type2_rate_probit <- tab_probit["1","0"] / sum(sub$MentalHealth_01 == 1)



```
