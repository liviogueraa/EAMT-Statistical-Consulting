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


# More elegant version
library(ggplot2)
library(patchwork)

# Create a shared theme to make text larger and clear for a screen
presentation_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    plot.margin = margin(10, 5, 10, 5) 
  )

# 1. Raw Data Plot
p1 <- ggplot(data, aes(x = keystrokes)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498db", color = "white") +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  presentation_theme +
  labs(title = "Original Data", 
       subtitle = "High skew & zero-inflation",
       x = "Keystrokes", y = "Density")

# 2. Log Plot
p2 <- ggplot(data, aes(x = log(keystrokes + 1))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#95a5a6", color = "white") +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  presentation_theme +
  labs(title = "Log(x + 1)", 
       subtitle = "Zero-spike persists",
       x = "Log(Keystrokes + 1)", y = "")

# 3. Square Root Plot
p3 <- ggplot(data, aes(x = sqrt(keystrokes))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#95a5a6", color = "white") +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  presentation_theme +
  labs(title = "Square Root", 
       subtitle = "Normality not achieved",
       x = "sqrt(Keystrokes)", y = "")

# Combined
combined_plot <- (p1 | p2 | p3) + 
  plot_annotation(
    title = 'Why a Tweedie Model? Comparing Data Transformations',
    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  )

# SHOW PLOT
combined_plot

ggsave("tweedie_justification.png", 
       plot = combined_plot, 
       width = 12,    
       height = 5,    
       dpi = 300)




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
    (1 | PET) +
    (1 | text_name / Sent_id),       
  family = tweedie(), 
  data = data
)

# 2. Check stability
library(DHARMa)
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


#### Simplifying the model to make residuals homoschedastic ####
# Creating the simplified version: no interaction term
reduced_model <- update(final_recovery_model, . ~ . - condition:domain)

# Compare them
anova(final_recovery_model, reduced_model)
# no significant differences in predictive power and AIC and BIC are lower, we keep the reduced

# Check Variance Inflation Factors
performance::check_collinearity(final_recovery_model) # since the full model has interaction terms. VIFs might be inflated
performance::check_collinearity(reduced_model) # here no highly correlated variables


# 2. Check stability
library(DHARMa)
final_recovery_model$sdr$pdHess
res <- simulateResiduals(reduced_model)
plot(res)


# Let's try to simplify some more
summary(reduced_model)


# no Difficulty term
reduced_model_noDiff <- update(reduced_model, . ~ . - scale(Difficulty))

res <- simulateResiduals(reduced_model_noDiff)
plot(res)
# now it works

summary(reduced_model_noDiff)


# Compare them
anova(reduced_model, reduced_model_noDiff)
# clear indication to stick with the second reduced model (no difficulty)

summary(reduced_model_noDiff)

#### Thus, the final model is reduced_model_noDiff ####

plot_model(reduced_model_noDiff, type = "re")


library(ggplot2)
library(dplyr)

# extract fixed effects table
coefs <- summary(reduced_model_noDiff)$coefficients$cond
df <- as.data.frame(coefs)

# add term names
df$term <- rownames(df)

# rename columns for convenience
df <- df %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    p.value = `Pr(>|z|)`
  )

# get confidence intervals (Wald = faster)
ci <- confint(reduced_model_noDiff, method = "Wald")

# keep only fixed effects (match row names)
ci <- as.data.frame(ci)
ci$term <- rownames(ci)

df <- df %>%
  left_join(ci, by = "term") %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

# remove intercept
df <- df %>%
  filter(term != "(Intercept)")

# relabel terms (optional but important for presentation)
df <- df %>%
  mutate(
    term = recode(term,
                  "condition2" = "Condition 2",
                  "condition3" = "Condition 3",
                  "condition4" = "Condition 4",
                  "domainnews" = "Domain: News",
                  "scale(num_characters)" = "Text length",
                  "scale(num_minor)" = "Minor errors",
                  "scale(num_major)" = "Major errors",
                  "scale(MT_Quality)" = "MT quality"
    ),
    significance = ifelse(p.value < 0.05, "Significant", "Not significant")
  )

library(ggplot2)
library(dplyr)
library(forcats)

# reorder: significant first, then by effect size
df <- df %>%
  mutate(significant_flag = p.value < 0.05) %>%
  arrange(desc(significant_flag), desc(abs(estimate))) %>%
  mutate(term = factor(term, levels = rev(term)))

# plot
p <- ggplot(df, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "gray50") +
  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 0.8, color = "gray40") +
  
  geom_point(aes(shape = significant_flag),
             size = 3, stroke = 1.1, color = "black", fill = "white") +
  
  scale_shape_manual(values = c(1, 16), guide = "none") +
  
  labs(
    title = "Effects on Keystrokes",
    subtitle = "Estimates with 95% confidence intervals (log scale)",
    x = "Effect size",
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85"),
    plot.margin = margin(10, 15, 10, 10)
  )

# display
p

# save 
ggsave(
  filename = "keystrokes_effects_plot.png",
  plot = p,
  width = 7,
  height = 4.5,
  dpi = 300
)


#### random interceptsplots ####
library(ggplot2)
library(glmmTMB)
library(sjPlot)
# Use the specific sjPlot function
sjPlot::plot_model(reduced_model_noDiff, type = "re", grid = FALSE)[[3]]+
  theme_minimal()+
  labs(x = "Text", y= "Random Intercept", title = "Random Effects: Texts")



# What is the statistical power of this model?
# Extract the power parameter (rho)
glmmTMB::family_params(reduced_model_noDiff) 
# This means we have some "zero-keystroke" tasks and the rest follow a skewed continuous distribution.
library(rlang)
library(broom.mixed)
library(ggplot2)
library(dplyr)

# 1. Extract the random effects into a tidy data frame
# 'condVar = TRUE' gets the standard errors for those "whiskers"
re_data <- tidy(reduced_model_noDiff, effects = "ran_vals", conf.int = TRUE)

# 2. Plotting
ggplot(re_data, aes(x = estimate, y = reorder(level, estimate), xmin = conf.low, xmax = conf.high)) +
  # Add a vertical line at 0 (the population average)
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # Use pointrange for the dots and the error bars
  geom_pointrange(color = "#2c3e50", size = 0.4) +
  # Facet by the grouping variable (PET vs Text vs Sentence)
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  # Clean up the look
  theme_minimal(base_size = 14) +
  labs(
    title = "Random Effects: Deviation from Population Average",
    subtitle = "Points represent shifts in baseline effort per participant and sentence",
    x = "Effect Size (Log Scale)",
    y = ""
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )




#### Comparing final model Vs other models ####

# Defining the core formula (all predictors)
main_formula <- keystrokes ~ condition + domain + scale(num_characters) + 
  scale(num_minor) + scale(num_major) + scale(MT_Quality) + 
  (1 | PET) + (1 | text_name / Sent_id)



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

# Even though the Hurdle model has a slightly lower AIC, 
# We would argue for the Tweedie model for three reasons:
# 1) The BIC weight for the Tweedie model is >.999. This is a massive statistical signal that the Tweedie model
# is the most parsimonious. 
# 2) The Tweedie model has the lowest RMSE (31.539). This means its actual predictions are closer to actual data points
# than the other two models.
# 3) In the Tweedie model, the Itra Class Correlation (ICC) is 0.548, while in the ZI model, it's 0.956, which is
# high and might suggests that the model is struggling to separate the random effects from the zero-inflation logic.

# For the Marginal R^2 (0.191): Fixed effects (condition, length, quality, etc.) explain about 19.1% of the variance 
# in keystrokes. 
# For Conditional R^2 (0.634): Adding the random part we are explaining nearly 64% of the variance.


exp(0.36379)
exp(-0.14641)

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



