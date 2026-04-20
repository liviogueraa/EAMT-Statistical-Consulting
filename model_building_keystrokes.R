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


#### visualising random intercepts ####
library(glmmTMB)
library(sjPlot)

# Assuming your model looks something like this:
# model <- glmmTMB(recovery_time ~ x1 + (1|group), 
#                  family = tweedie(), data = df)

# This creates the "Caterpillar plot" automatically
plot_model(final_recovery_model, type = "re")



#### Does condition matter overall? ####
car::Anova(final_recovery_model, type = "III")


#### Simplifying the model ####
# Creating the simplified version: no interaction term
reduced_model <- update(final_recovery_model, . ~ . - condition:domain)

# Compare them
anova(final_recovery_model, reduced_model)
# no significant differences in predictive power and AIC and BIC are lower, we keep the reduced

# Check Variance Inflation Factors
performance::check_collinearity(final_recovery_model) # since the full model has interaction terms. VIFs might be inflated
performance::check_collinearity(reduced_model) # here no highly correlated variables


# Let's try to simplify some more
summary(reduced_model)

# no Difficulty term
reduced_model_noDiff <- update(reduced_model, . ~ . - scale(Difficulty))

# Compare them
anova(reduced_model, reduced_model_noDiff)
# clear indication to stick with the second reduced model (no difficulty)

summary(reduced_model_noDiff)

# What is the statistical power of this model?
# Extract the power parameter (rho)
glmmTMB::family_params(reduced_model_noDiff) 
# This means we have some "zero-keystroke" tasks and the rest follow a skewed continuous distribution.


#### Comparing final model Vs other models ####

# Defining the core formula (all predictors)
main_formula <- keystrokes ~ condition + domain + scale(num_characters) + 
  scale(num_minor) + scale(num_major) + scale(MT_Quality) + 
  (1 | PET) + (1 | text_name)


# 1. Current Tweedie Model
mod_tweedie <- glmmTMB(main_formula, 
                       family = tweedie(link = "log"), 
                       data = data)

# 2. Zero-Inflated Negative Binomial
# (Assumes some zeros come from a separate 'always zero' process)
mod_zinb <- glmmTMB(main_formula, 
                    ziformula = ~1, 
                    family = nbinom2, 
                    data = data)

# 3. Hurdle Model (Truncated Negative Binomial)
# (Assumes a 'gatekeeper' process: 0 vs >0, then counts for the rest)
mod_hurdle <- glmmTMB(main_formula, 
                      ziformula = ~., 
                      family = truncated_nbinom2, 
                      data = data)

library(performance)

# Compare them side-by-side
comparison <- compare_performance(mod_tweedie, mod_zinb, mod_hurdle, metrics = "common")
print(comparison)


# The 'rho' (power parameter)
glmmTMB::family_params(mod_tweedie)

# Even though the Hurdle model has a slightly lower AIC (a difference of ~2.2 is very small), 
# We would argue for the Tweedie model for three reasons:
# 1) The BIC weight for the Tweedie model is >.999. This is a massive statistical signal that the Tweedie model
# is the most parsimonious. 
# 2) The Tweedie model has the lowest RMSE (38.347). This means its actual predictions are closer to actual data points
# than the other two models.
# 3) In the Tweedie model, the Itra Class Correlation (ICC) is 0.367, while in the ZI model, it's 0.922, which is
# high and often suggests the model is struggling to separate the random effects from the zero-inflation logic.

# For the Marginal R^2 (0.205): Fixed effects (condition, length, quality, etc.) explain about 20.5% of the variance 
# in keystrokes. 
# For Conditional R^2 (0.497): When we add in the individual differences of the translators and the specific 
# texts, we are explaining nearly 50% of the variance.



#### Comparing models with and w/o time = 0 obs ####

data_nonzero_time <- data %>%
  filter(time != 0)

# what is the new distribution of keystrokes?
hist(data_nonzero_time$keystrokes,
     probability = TRUE,
     main = "Histogram with Density Curve - raw data",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(data_nonzero_time$keystrokes),
      col = "red",
      lwd = 2)


# does the square root work now in this case?
hist(sqrt(data_nonzero_time$keystrokes),
     probability = TRUE,
     main = "Histogram with Density Curve - Sqrt",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(sqrt(data_nonzero_time$keystrokes)),
      col = "red",
      lwd = 2)



# does the log work now in this case?
hist(log(data_nonzero_time$keystrokes+1),
     probability = TRUE,
     main = "Histogram with Density Curve - Log",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(log(data_nonzero_time$keystrokes+1)),
      col = "red",
      lwd = 2)



# Are the relative models well-defined?

# let's see if LMM works with sqrt (check residuals)
sqrt_keystrokes_model <- lmer(scondition * domain + 
                                scale(num_characters) + 
                                scale(num_minor) + 
                                scale(num_major) + 
                                scale(MT_Quality) + 
                                scale(Difficulty) + 
                                (1 | PET) +            
                                (1 | text_name), 
                             data = data_nonzero_time, 
                             REML = FALSE)
summary(sqrt_keystrokes_model)

vcov(sqrt_keystrokes_model)

library(lmerTest) # Load this first
# Then re-run your model code
summary(sqrt_keystrokes_model)

# checking the residuals
library(performance)
library(see)

# Basic residual vs. fitted plot
plot(sqrt_keystrokes_model)

# Q-Q plot for residuals
library(lattice)
qqmath(sqrt_keystrokes_model)

# Q-Q plots for all random effects
dotplot(ranef(sqrt_keystrokes_model, condVar = TRUE))

# check for outliers
# influence(sqrt_keystrokes_model)
library(lme4)
library(lattice)
dotplot(ranef(sqrt_keystrokes_model))



# let's see if LMM works with log (check residuals)
log_keystrokes_model <- lmer(log(keystrokes+1) ~ 
                               condition * domain + 
                               scale(num_characters) + 
                               scale(num_minor) + 
                               scale(num_major) + 
                               scale(MT_Quality) + 
                               scale(Difficulty) + 
                               (1 | PET) +            
                               (1 | text_name), 
                   data = data_nonzero_time, 
                   REML = FALSE)

summary(log_keystrokes_model)



# checking the residuals
library(performance)
library(see)

# Basic residual vs. fitted plot
plot(log_keystrokes_model)

# Q-Q plot for residuals
library(lattice)
qqmath(log_keystrokes_model)

# Q-Q plots for all random effects
dotplot(ranef(log_keystrokes_model, condVar = TRUE))

# check for outliers
# influence(sqrt_keystrokes_model)
library(lme4)
library(lattice)
dotplot(ranef(sqrt_keystrokes_model))



vcov(log_keystrokes_model)

nonzero_time_model <- glmmTMB(
  keystrokes ~ condition * domain + 
    scale(num_characters) + 
    scale(num_minor) + 
    scale(num_major) + 
    scale(MT_Quality) + 
    scale(Difficulty) + 
    (1 | PET) +            # Essential: Translator variance
    (1 | text_name),        # Essential: Text difficulty variance
  family = tweedie(), 
  data = data_nonzero_time
)

nonzero_time_model$sdr$pdHess
res <- simulateResiduals(nonzero_time_model)
plot(res)



## BOX COX 

library(MASS)

# 1. Run the boxcox on a standard lm version of your model
# Note: Use + 1 if you have any zeros in your data
library(MASS)
bc <- boxcox(keystrokes + 1 ~ condition * domain + num_characters + 
               num_minor + num_major + MT_Quality + Difficulty, 
             data = data_nonzero_time)

# 2. Extract the lambda that maximizes the log-likelihood
optimal_lambda <- bc$x[which.max(bc$y)]
print(optimal_lambda)
# it suggests to use log(x+1)

######################
#  COMPARISON OF TIMES

data_time <- read.csv("data/data_time.csv")
hist(data_time$time,
     probability = TRUE,
     main = "Histogram with Density Curve",
     xlab = "Keystrokes",
     col = "lightgray",
     border = "white")

lines(density(data_time$time),
      col = "red",
      lwd = 2)

library(ggplot2)
library(dplyr)

# 1. Prepare the comparison data
# Assuming 'data' is your main dataset and 'data_time' contains the imputed values
original_time <- data %>% 
  filter(time > 0) %>% 
  select(time, condition) %>% 
  mutate(Source = "Original")

imputed_time <- data_time %>% 
  select(time, condition) %>% 
  mutate(Source = "Imputed")

# Combine them for comparison
comp_df <- rbind(original_time, imputed_time)

# 2. Visual Check: Density plots per condition
ggplot(comp_df, aes(x = time, fill = Source)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~condition) +
  theme_minimal() +
  labs(title = "Comparison of Original vs. Imputed Time Distributions",
       x = "Time (seconds)", y = "Density")

# 3. Statistical Check: Descriptive Stats
summary_stats <- comp_df %>%
  group_by(condition, Source) %>%
  summarise(
    N = n(),
    Mean = mean(time),
    Median = median(time),
    SD = sd(time)
  )
print(summary_stats)

# 4. Statistical Check: Wilcoxon Test (Non-parametric)
# Check overall if distributions differ
wilcox.test(time ~ Source, data = comp_df)

# Check per condition using a loop
lapply(split(comp_df, comp_df$condition), function(d) {
  wilcox.test(time ~ Source, data = d)
})



