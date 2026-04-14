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

# Factorization of categorical variables

data$PET <- as.factor(data$PET)
data$condition <- as.factor(data$condition)
data$domain <- as.factor(data$domain)
data$text_name <- as.factor(data$text_name)
data$Sent_id <- as.factor(data$Sent_id)

data$MT_Quality <- as.numeric(data$`How good was the quality of MT?`)
data$Difficulty <- as.numeric(data$`How difficult to translate was the source text?`)

# Analysis of outcome variable distribution

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

# Model building


model_full <- lmer(log(time) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                     (1 | PET) + (1 | text_name), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_full)

model_2 <- lmer(log(time) ~ 
                     condition * domain + num_characters + num_minor + num_major + MT_Quality + 
                     (1 | PET) + (1 | text_name), 
                   data = data_clean, 
                   REML = FALSE)

summary(model_2)
