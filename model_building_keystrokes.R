# Required Packages
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(see)
library(patchwork)
library(lme4)

# Loading dataset
data <- read_csv("data/data.csv")

#=======
# Factorization and Ordering of categorical variables

data$`How good was the quality of MT?` <- factor(
  data$`How good was the quality of MT?`, 
  levels = c("Insufficient", "Fair", "Good", "Very good"),
  ordered = TRUE
)

data$`How difficult to translate was the source text?` <- factor(
  data$`How difficult to translate was the source text?`, 
  levels = c("Very difficult", "Somewhat difficult", "Neutral",
             "Somewhat easy", "Very easy"),
  ordered = TRUE
)

data$`How useful were the error annotations?` <- factor(
  data$`How useful were the error annotations?`, 
  levels = c("nan", "Extremely not useful", "Somewhat not useful",
             "Neutral", "Somewhat useful", "Extremely useful"),
  ordered = TRUE
)

data$`How useful were the error annotations?` <- factor(
  data$`How useful were the error annotations?`, 
  levels = c("nan", "Extremely not useful", "Somewhat not useful",
             "Neutral", "Somewhat useful", "Extremely useful"),
  ordered = TRUE
)

data$`How accurate were the error annotations?` <- factor(
  data$`How accurate were the error annotations?`, 
  levels = c("nan", "Somewhat inaccurate", "Not accurate nor inaccurate",
             "Somewhat accurate"),
  ordered = TRUE
)

data$`How useful were the translation suggestions?` <- factor(
  data$`How useful were the translation suggestions?`, 
  levels = c("nan", "Neutral", "Somewhat useful",
             "Extremely useful"),
  ordered = TRUE
)

data$`How accurate were the translation suggestions?` <- factor(
  data$`How accurate were the translation suggestions?`, 
  levels = c("nan", "Somewhat inaccurate", "Not accurate nor inaccurate",
             "Somewhat accurate", "Very accurate"),
  ordered = TRUE
)


#str(data)
data$PET <- as.factor(data$PET)
data$condition <- as.factor(data$condition)
data$domain <- as.factor(data$domain)
data$text_name <- as.factor(data$text_name)
data$Sent_id <- as.factor(data$Sent_id)

data$MT_Quality <- as.numeric(data$`How good was the quality of MT?`)
data$Difficulty <- as.numeric(data$`How difficult to translate was the source text?`)
data$condition <- factor(data$condition)

#### Y variable transformation ####
# raw data
ggplot(data, aes(x = keystrokes)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Keystrokes",
       x = "Keystrokes",
       y = "Density")

# TRANSFORMATIONS
using_log <- log(data$keystrokes + 1) # the 1 is to avoid + infinity for the zero values

hist(using_log,
     probability = TRUE,
     main = "Histogram with Density Curve",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(using_log),
      col = "red",
      lwd = 2)


# see with square root
radice_key <- sqrt(data$keystrokes)
hist(radice_key,
     probability = TRUE,
     main = "Histogram with Density Curve",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(radice_key),
      col = "red",
      lwd = 2)


# box cox
library(MASS)
dummy_data <- data
dummy_data$keystrokes_shift <- (data$keystrokes + 1)
boxcox(lm(keystrokes_shift ~ 1, data = dummy_data))

any(is.na(dummy_data$keystrokes_shift))
any(is.infinite(dummy_data$keystrokes_shift))

model <- lm(keystrokes_shift ~ 1, data = dummy_data)
str(model)

bc <- MASS::boxcox(model, plotit = FALSE)

lambda_opt <- bc$x[which.max(bc$y)]
lambda_opt




library(bestNormalize)
yj <- yeojohnson(dummy_data$keystrokes)
data$keystrokes_yj <- predict(yj)


hist(data$keystrokes_yj,
     probability = TRUE,
     main = "Histogram with Density Curve",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(data$keystrokes_yj),
      col = "red",
      lwd = 2)


inspection <- data[data$keystrokes == 0,]
nrow(inspection[inspection$condition != 4,])

no_key_cond_4 <- inspection[inspection$condition == 4,]

tags_edited_no_keystrokes <- inspection[!is.na(inspection$tags_edited),] # strange but possibly these two cases were words dragged using the mouse(?)
nrow(data[data$keystrokes == 0,])
nrow(data[data$time == 0,])

no_time <- data[data$time == 0,]

aa <- data[data$prc_sugg_accepted != "N/A",]





#### Tweedie GLMM ####

# The Tweedie distribution is a member of the Exponential Dispersion Family. Here, it is specifically a 
# Compound Poisson-Gamma distribution

# It acts as a single mathematical framework that handles two processes at once:
# 1) The Discrete Part (Poisson): This models the probability of an event occurring (e.g., whether the 
#  translator chooses to edit the sentence or leave it at 0 keystrokes);
# The Continuous Part (Gamma): This models the magnitude of the event (e.g., if they do edit, how many keystrokes are required)


# CONFRONTARE CON INTERACTION DOMAIN*CONDITION
# AGGIUNGERE TUTTE LE VARIABILI E FARE PROCEDURA SELEZIONE

library(glmmTMB) # CHECK ZERO INFLATION MODELS OR TWO PART WITH SOME MODEL FOR 0 AND GAMMA FOR THE REST
full_model <- glmmTMB(keystrokes ~ condition * domain + num_characters + num_minor + num_major + MT_Quality + Difficulty + 
                   (1 | PET) + (1 | text_name) + (1 | Sent_id), 
                 family = tweedie(), data = data, REML = FALSE)
summary(full_model)

full_model$sdr$pdHess


library(DHARMa)
res <- simulateResiduals(full_model)
plot(res)

# A more robust, simplified model
refined_model <- glmmTMB(keystrokes ~ condition * domain + 
                             num_characters + num_minor + num_major + 
                             MT_Quality + Difficulty + 
                             (1 | PET) + (1 | text_name), 
                           family = tweedie(), 
                           data = data)

summary(refined_model)


library(glmmTMB)

# 1. Scale variables within the call to ensure numerical stability
final_recovery_model <- glmmTMB(
  keystrokes ~ condition * domain + 
    scale(num_characters) + 
    scale(num_minor) + 
    scale(num_major) + 
    scale(MT_Quality) + 
    scale(Difficulty) + 
    (1 | PET) +            # Essential: Translator variance
    (1 | text_name),        # Essential: Text difficulty variance
  family = tweedie(), 
  data = data
)

# 2. Check stability
final_recovery_model$sdr$pdHess
res <- simulateResiduals(final_recovery_model)
plot(res)

summary(final_recovery_model)

# Let's look at the variance-covariance matrix for each random effect group
VarCorr(final_recovery_model)

# Returns the vcov matrix for the fixed effects
vcov(final_recovery_model)$cond
