################################################################################
# -------------------------- MODEL BUILDING - TIME --------------------------- #
################################################################################

# Required Packages

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(see)
library(patchwork)
library(lme4)
library(lmerTest)
library(lattice)
library(sjPlot)  
library(mice)
library(performance)

# Factorization of categorical variables

data$PET <- as.factor(data$PET)
data$condition <- as.factor(data$condition)
data$domain <- as.factor(data$domain)
data$text_name <- as.factor(data$text_name)
data$Sent_id <- as.factor(data$Sent_id)

data$MT_Quality <- as.numeric(data$`How good was the quality of MT?`)
data$Difficulty <- as.numeric(data$`How difficult to translate was the source text?`)

# Analysis of outcome variable distribution

View(subset(data, condition == 1 & domain == "medical"))

data_clean <- data %>%
  filter(time > 1)

p1 <- ggplot(data_clean, aes(x = time)) +
  geom_histogram(aes(y = ..density..), bins = 30, 
                 fill = "#69b3a2", color = "white", alpha = 0.7) +
  geom_density(color = "#404040", size = 1) +
  theme_modern() +
  labs(title = "Histogram of Elapsed Time",
       x = "Time (seconds)", y = "Density")

p2 <- ggplot(data_clean, aes(x = sqrt(time))) +
  geom_histogram(aes(y = ..density..), bins = 30, 
                 fill = "#40739e", color = "white", alpha = 0.7) +
  geom_density(color = "#404040", size = 1) +
  theme_modern() +
  labs(title = "Histogram of Elapsed Time after log-transformation",
       x = "Time (log-seconds)", y = "Density")

p1 + p2

# Model building - Full data

model_full <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name) + (1 | Sent_id), 
                   data = data, 
                   REML = FALSE)

summary(model_full)

model_2 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET), 
                data = data, 
                REML = FALSE)

summary(model_2)

anova(model_full, model_2)

model_3 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET), 
                data = data, 
                REML = TRUE)
summary(model_3)  

icc_results <- icc(model_3)

print(icc_results) 

r2_results <- r2(model_3)

print(r2_results) 

plot(model_3, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "red")

pet_re <- ranef(model_3)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")


# Model building - data_clean

model_full <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name) + (1 | Sent_id), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_full)

model_2 <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                     (1 | PET) + (1 | text_name), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_2)

anova(model_full, model_2)

model_3 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET) + (1 | text_name), 
                data = data_clean, 
                REML = TRUE)
summary(model_3)  
# Random effects:
# PET Variance is quite high, meaning some translators are generally faster than others
# Text variance is instead lower, texts were balanced
# Residual is the largest, which is expected when measuring human velocity

# Fixed effects
# to interpret using the inverse formula exp(est) - 1 
# (at baseline level is for medical domain only) 
# (to interpret news we have to add interactions)
# Condition 2 is the strongest effect, reducing time from baseline by 44%
# Condition 3 by 32%
# Condition 4 by 25%
# all 3 are significative at alpha = 0.05 level

# num characters/ num minor, major/ mt quality are all relevant controls

# On news domain apparently condition 4 is the fastest while others actually get slower

icc_results <- icc(model_3)

print(icc_results) # 13% of the total variance is explained by the Random effects  

r2_results <- r2(model_3)

print(r2_results) # 30% of the total variance is explained by the fixed effects

# In total the model acocunts for 43% of the variance


plot(model_3, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "red")

pet_re <- ranef(model_3)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")


## test with time = 0 substituted with number of characters / 20


data_impute <- data
data_impute$time_fixed <- ifelse(data_impute$time == 0, data_impute$num_characters / 20, data_impute$time)
View(data_impute)

model_fixed <- lmer(log(time_fixed) ~ 
                      condition * domain + num_characters + 
                      num_minor + num_major + MT_Quality + 
                      (1 | PET) + (1 | text_name), 
                    data = data_impute, REML = TRUE)

summary(model_fixed)

icc_results1 <- icc(model_fixed)

print(icc_results1) 

r2_results1 <- r2(model_fixed)

print(r2_results1) 

plot(model_fixed, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_fixed))
qqline(residuals(model_fixed), col = "red")

pet_re <- ranef(model_fixed)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")
