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
library(ggeffects)

# Factorization of categorical variables

data$PET <- as.factor(data$PET)
data$condition <- as.factor(data$condition)
data$domain <- as.factor(data$domain)
data$text_name <- as.factor(data$text_name)
data$Sent_id <- as.factor(data$Sent_id)

data$MT_Quality <- as.numeric(data$`How good was the quality of MT?`)
data$Difficulty <- as.numeric(data$`How difficult to translate was the source text?`)

# Analysis of outcome variable distribution

zeros<-data[data$time == 0,]
table(zeros$condition, zeros$domain)
table(data$condition, data$domain)

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

#######################################
# Model building - Full raw data
#######################################

model_full <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name / Sent_id), 
                   data = data, 
                   REML = FALSE)

summary(model_full)

model_2 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data, 
                REML = FALSE)

summary(model_2)

anova(model_full, model_2)

model_2.1 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data, 
                REML = FALSE)

summary(model_2.1)

anova(model_2, model_2.1)

model_3 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data, 
                REML = TRUE)
summary(model_3)  
# None of the condition effects are statistically relevant
# only one close is condition 2 in medical texts, whose interval is almost all 
# below 0

eff_int_raw <- predict_response(model_3, terms = c("condition", "domain"))
plot(eff_int_raw) + 
  labs(
    title = "Predicted Translation Time by Condition and Domain",
    subtitle = "Values back-transformed from Log to Seconds",
    x = "Condition", 
    y = "Estimated Time (Seconds)",
    colour = "Domain"
  ) +
  theme_minimal()

plot_model(model_3, 
           type = "est", 
           show.values = TRUE, 
           value.offset = .3,
           title = "Fixed Effects Estimates (Log Scale)")

icc_results <- icc(model_3)

print(icc_results) 

r2_results <- r2(model_3)

print(r2_results) 

plot(model_3, which = 1, main = "Residuals vs Fitted") 
# There is a systematic pattern of errors due to the presence of time = 0 observations
# violating the assumption of homoscedasticity (constant variance of errors)
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "red")
# The QQplot is completely skewed in lower values aswell
# residuals are NOT normally distributed

# therefore results are unreliable


#######################################
# Model building - Dataset with removed 0 time instances
#######################################

model_full <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name : Sent_id), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_full)

model_2 <- lmer(log(time+1) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                     (1 | PET) + (1 | text_name : Sent_id), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_2)

anova(model_full, model_2)

model_3 <- lmer(log(time+1) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data_clean, 
                REML = TRUE)
summary(model_3)  

icc_results <- icc(model_3)

print(icc_results) # 22% of the total variance is explained by the Random effects  

r2_results <- r2(model_3)

print(r2_results) # 30% of the total variance is explained by the fixed effects

# In total the model acocunts for 51% of the variance


plot(model_3, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "red")
# Assumptions checked, residuals are normally distributed

pet_re <- ranef(model_3)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")

# limit of this model is: 
# removing zeros is CONCEPTUALLY WRONG 
# as they are not MCAR (missing completely at random)
# this causes SELECTION BIAS

#######################################
# Model building - dataset with 0 time instances substituted with number of characters/20
#######################################
# Under the assumption a professional translator takes around 1 second to make sure 
# 20 characters contain no errors and should not be edited

data_impute <- data
data_impute$time_fixed <- ifelse(data_impute$time == 0, data_impute$num_characters / 20, data_impute$time)
View(data_impute[data_impute$time == 0,])

model_fixed <- lmer(log(time_fixed) ~ 
                      condition * domain + num_characters + 
                      num_minor + num_major + MT_Quality + 
                      (1 | PET) + (1 | text_name : Sent_id), 
                    data = data_impute, REML = TRUE)

summary(model_fixed)
# Condition 2 effect is statistically significant in medical domain
# and lowers elapsed time by 27% compared to baseline

eff_int <- predict_response(model_fixed, terms = c("condition", "domain"))

plot(eff_int) + 
  labs(
    title = "Predicted Translation Time by Condition and Domain",
    subtitle = "Values back-transformed from Log to Seconds",
    x = "Condition", 
    y = "Estimated Time (Seconds)",
    colour = "Domain"
  ) +
  theme_minimal()

plot_model(model_fixed, 
           type = "est", 
           show.values = TRUE, 
           value.offset = .3,
           title = "Fixed Effects Estimates (Log Scale)")

icc_results1 <- icc(model_fixed)

print(icc_results1) # Random effects account for 18% of total variance

r2_results1 <- r2(model_fixed)

print(r2_results1) # 31% of total variance comes from fixed effects
# R2 is 49%

plot(model_fixed, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_fixed))
qqline(residuals(model_fixed), col = "red")
# Clear lower tail, the model overestimates lower times
# But generally acceptable diagnostics

pet_re <- ranef(model_fixed)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")
random_effects <- ranef(model_fixed)
plot_model(model_fixed, type = "re", grid = FALSE)[[2]] + 
  theme_minimal() +
  labs(title = "Random Effects: PET (Translators)")
plot_model(model_fixed, type = "re", grid = FALSE)[[1]] + 
  theme_minimal() +
  labs(title = "Random Effects: text_name")

data_impute$condition <- relevel(as.factor(data_impute$condition), ref = "2")
model_fixed <- lmer(log(time_fixed) ~ 
                      condition * domain + num_characters + 
                      num_minor + num_major + MT_Quality + 
                      (1 | PET) + (1 | text_name : Sent_id), 
                    data = data_impute, REML = TRUE)

summary(model_fixed)


#######################################
# Model building - dataset with instances with time = 0 substituted with newly proposed times
#######################################

data_time <- read_csv("data/data_time.csv")
View(data_time)

indices_zeros <- which(data$time == 0)

data$time_hybrid <- data$time
data$time_hybrid[indices_zeros] <- data_time$time

data_clean2 <- data[data$time_hybrid>1,]

model_hyb <- lmer(log(time_hybrid) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name / Sent_id), 
                   data = data_clean2, 
                   REML = FALSE)

summary(model_hyb)

model_hyb2 <- lmer(log(time_hybrid) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data_clean2, 
                REML = FALSE)

summary(model_hyb2)


model_hyb3 <- lmer(log(time_hybrid) ~ 
                  condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                  (1 | PET) + (1 | text_name : Sent_id), 
                data = data_clean2, 
                REML = TRUE)
summary(model_hyb3)
# In medical texts all condition effects are statistically relevant compared to the baseline
# Condition 2 reduces time by 40.6% in medical texts
# condition 3 by 31.1% in medical texts
# condition 4 by 27.5% in medical and news texts

eff_int_hyb <- predict_response(model_hyb3, terms = c("condition", "domain"))
plot(eff_int_hyb) + 
  labs(
    title = "Predicted Translation Time by Condition and Domain",
    subtitle = "Values back-transformed from Log to Seconds",
    x = "Condition", 
    y = "Estimated Time (Seconds)",
    colour = "Domain"
  ) +
  theme_minimal()

plot_model(model_hyb3, 
           type = "est", 
           show.values = TRUE, 
           value.offset = .3,
           title = "Fixed Effects Estimates (Log Scale)")


icc_results2 <- icc(model_hyb3) 

print(icc_results2) #random part accounts for 21% of total variance

r2_results2 <- r2(model_hyb3) # fixed effects are 28% of total variance

print(r2_results2) 
# R2 is 49%

plot(model_hyb3, which = 1, main = "Residuals vs Fitted")
qqnorm(residuals(model_hyb3))
qqline(residuals(model_hyb3), col = "red")
# residuals are normally distributed

pet_re <- ranef(model_hyb3)$PET[,1]
qqnorm(pet_re, main = "Q-Q Plot: Random Effects (PET)")
qqline(pet_re, col = "red")
random_effects <- ranef(model_hyb3)
plot_model(model_hyb3, type = "re", grid = FALSE)[[2]] + 
  theme_minimal() +
  labs(title = "Random Effects: PET (Translators)")
plot_model(model_hyb3, type = "re", grid = FALSE)[[1]] + 
  theme_minimal() +
  labs(title = "Random Effects: sentence ID")

# Limit of this analysis is that the newly imputed times are measured differently than the originals
# they may be affected by external factors, such as translator taking breaks, and 
# longer time to actually start the task

data_clean2$condition <- relevel(as.factor(data_clean2$condition), ref = "2")
model_hyb3 <- lmer(log(time_hybrid) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                     (1 | PET)+ (1 | text_name : Sent_id), 
                   data = data_clean2, 
                   REML = TRUE)
summary(model_hyb3)


#####################
#library(glmmTMB)
#library(DHARMa)

#data_clean2$productivity <- data_clean2$keystrokes / data_clean2$time_hybrid

#model_prod <- glmmTMB(
#  productivity+1 ~ condition * domain + MT_Quality + 
#    (1 | PET)+ (1 | text_name : Sent_id), 
#  data = data_clean2,
#  family = tweedie(link = "log"),
#  control = glmmTMBControl(optimizer = optim, 
#                           optArgs = list(method = "BFGS"))
#)
