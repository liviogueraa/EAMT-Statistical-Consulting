############################################################################
# ----- THIS SECTION IS DEDICATED TO A PRIOR ANALYSIS OF THE DATASET ----- #
############################################################################

#=======
# Required Packages
library(readr)
library(ggplot2)
library(dplyr)
library(see)
library(patchwork)

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
  levels = c("Very easy", "Somewhat easy", "Neutral",
             "Somewhat difficult", "Very difficult"),
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

# Creating tables per variable
data_categorical <- data[,-c(7,8,9,10,11,12,13)]
lapply(data_categorical[,-7], table)

#=======
# Tables

table(data$PET, data$`How accurate were the translation suggestions?`)
table(data$PET, data$`How useful were the translation suggestions?`)
table(data$PET, data$`How good was the quality of MT?`)
table(data$`How good was the quality of MT?`, data$condition)
table(data$text_name, data$`How good was the quality of MT?`)

data %>%
  group_by(`How good was the quality of MT?`) %>%
  summarise(mean_time = mean(time, na.rm = TRUE),
            mean_keys = mean(keystrokes, na.rm = TRUE),
            n = n())

data %>%
  group_by(condition) %>%
  summarise(mean_time = mean(time),
            mean_keys = mean(keystrokes),
            n = n())

table(data$condition, data$`How good was the quality of MT?`)
table(data$condition, data$`How difficult to translate was the source text?`)

table(data$condition, data$num_major)
table(data$condition, data$num_minor)
#=======
# Figures
# Boxplots of Keystrokes and Time per Condition

timebp <- ggplot(data = data, mapping = aes(x = as.factor(condition),
                                            y = time, fill = as.factor(condition))) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_okabeito(order = 3:6) +
  labs(title = "Temporal Efficiency per Condition",
       x = "Condition",
       y = "Time (seconds)") +
  theme(legend.position = "none")

keybp <- ggplot(data = data, mapping = aes(x = as.factor(condition), 
                                           y = keystrokes, fill = as.factor(condition))) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_okabeito(order = 3:6) +
  labs(title = "Number of Keystrokes per Condition",
       x = "Condition",
       y = "Number of Keystrokes") +
  theme(legend.position = "none")

timebp/keybp

# Boxplots of Keystrokes and Time per Condition for each Professional Translator

timebp_faceted <- ggplot(data = data, aes(x = as.factor(condition), y = time)) + 
  geom_boxplot(aes(fill = as.factor(condition)), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) + 
  facet_wrap(~ PET, ncol = 4) + 
  
  theme_minimal() +
  scale_fill_okabeito(order = 3:6) + 
  
  labs(title = "Temporal Efficiency per Condition",
       subtitle = "Divided by Professional Translator",
       x = "Condition",
       y = "Time (Seconds)") +
  
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "gray90"), 
        panel.spacing = unit(1, "lines")) 

print(timebp_faceted)

keybp_faceted <- ggplot(data = data, aes(x = as.factor(condition), y = keystrokes)) + 
  geom_boxplot(aes(fill = as.factor(condition)), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) + 
  facet_wrap(~ PET, ncol = 4) + 
  
  theme_minimal() +
  scale_fill_okabeito(order = 3:6) + 
  
  labs(title = "Number of Keystrokes per Condition",
       subtitle = "Divided by Professional Translator",
       x = "Condition",
       y = "Number of Keystrokes") +
  
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "gray90"), 
        panel.spacing = unit(1, "lines")) 

print(keybp_faceted)

