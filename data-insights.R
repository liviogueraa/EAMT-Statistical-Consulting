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

# Creating tables per variable
data_categorical <- data[,-c(7,8,9,10,11,12,13)]
lapply(data_categorical[,-7], table)

#=======
# Tables

table(data$condition, data$text_name)

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

table(data$condition, data$`How difficult to translate was the source text?`)

table(data$condition, data$num_major)
table(data$condition, data$num_minor)

#=======
# Figures
#
# Boxplots of Keystrokes and Time per Condition
#

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

#
# Boxplots of Keystrokes and Time per Condition for each Professional Translator
#

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

#
# Keystrokes and Time x Domain
#

domainboxplots <- ggplot(data = data, mapping = aes(x = as.factor(domain),
                                            y = log(time), fill = as.factor(domain))) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_okabeito(order = 2:3) +
  labs(title = "Temporal Efficiency per Domain",
       x = "Domain",
       y = "Log Time (seconds)") +
  theme(legend.position = "none")

domainboxplots2 <- ggplot(data = data, mapping = aes(x = as.factor(domain),
                                                    y = log(keystrokes), fill = as.factor(domain))) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_okabeito(order = 2:3) +
  labs(title = "Number of Keystrokes per Domain",
       x = "Domain",
       y = "Log Number of Keystrokes") +
  theme(legend.position = "none")

domainboxplots + domainboxplots2

#
# Keystrokes and Time x Domain x PET
#

domain_time_faceted <- ggplot(data = data, aes(x = as.factor(domain), y = time)) + 
  geom_boxplot(aes(fill = as.factor(domain)), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) + 
  facet_wrap(~ PET, ncol = 4) + 
  
  theme_minimal() +
  scale_fill_okabeito(order = 1:2) + 
  
  labs(title = "Temporal Efficiency per Domain",
       subtitle = "Divided by Professional Translator",
       x = "Domain",
       y = "Time (Seconds)") +
  
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "gray90"), 
        panel.spacing = unit(1, "lines")) 
domain_time_faceted

domain_key_faceted <- ggplot(data = data, aes(x = as.factor(domain), y = keystrokes)) + 
  geom_boxplot(aes(fill = as.factor(domain)), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1) + 
  facet_wrap(~ PET, ncol = 4) + 
  
  theme_minimal() +
  scale_fill_okabeito(order = 1:2) + 
  
  labs(title = "Number of Keystrokes per Domain",
       subtitle = "Divided by Professional Translator",
       x = "Domain",
       y = "Number of Keystrokes") +
  
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "gray90"), 
        panel.spacing = unit(1, "lines")) 
domain_key_faceted

# 
# Boxplots for each text
#

texts_boxplots_time <- ggplot(data, aes(x = text_name, y = time, fill = as.factor(condition))) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  facet_wrap(~ domain, scales = "free_x") + 
  
  theme_minimal() +
  scale_fill_okabeito(order = 3:6) + 
  
  labs(title = "Time distribution divided by Document and Condition",
       subtitle = "(Medical vs News)",
       x = "Document (Text Name)",
       y = "Time (Seconds)",
       fill = "Condition") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.background = element_rect(fill = "gray90"))

print(texts_boxplots_time)

#
# Stacked Bar Chart of Perceived Quality of MT
#

ggplot(data, aes(x = condition, fill = `How good was the quality of MT?`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Percieved Quality of MT per Condition",
       y = "Percentage", x = "Condition")

table(data$text_name, data$condition)
ggplot(data, aes(x = text_name, y = log(time), color = condition)) +
  geom_boxplot() +
  facet_wrap(~domain, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Intra-text correlation check",
       subtitle = "Ognuno di questi box rappresenta la correlazione che vogliamo gestire")

#
# Testing correlation between continuous variables and outcomes
#

plot_data <- data %>%
  mutate(
    log_time = log(time),
    log_keystrokes = log(keystrokes),
    highlight_ratio = as.numeric(as.character(highlight_ratio)),
    num_characters = as.numeric(as.character(num_characters)),
    condition = as.factor(condition)
  ) %>%
  filter(!is.na(highlight_ratio), !is.na(num_characters))

# 1. Time vs Highlight Ratio
p1 <- ggplot(plot_data, aes(x = highlight_ratio, y = log_time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() + labs(title = "Time vs Highlight Ratio", y = "log(Time)")

# 2. Keystrokes vs Highlight Ratio
p2 <- ggplot(plot_data, aes(x = highlight_ratio, y = log_keystrokes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() + labs(title = "Keystrokes vs Highlight Ratio", y = "log(Keystrokes)")

# 3. Time vs Num Characters
p3 <- ggplot(plot_data, aes(x = num_characters, y = log_time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() + labs(title = "Time vs Sentence Length", x = "Num Characters", y = "log(Time)")

# 4. Keystrokes vs Num Characters
p4 <- ggplot(plot_data, aes(x = num_characters, y = log_keystrokes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() + labs(title = "Keystrokes vs Sentence Length", x = "Num Characters", y = "log(Keystrokes)")

(p1 | p2) / (p3 | p4) + plot_layout(guides = 'collect')

# 
# Number of major and Minor errors compared to time and Keystrokes
#

# 1. Time vs Highlight Ratio
p11 <- ggplot(plot_data, aes(x = num_major, y = log_time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, col = "firebrick1") +
  theme_minimal() + labs(title = "Time vs Major Errors", x = "Num of Minor Errors", y = "log(Time)")

# 2. Keystrokes vs Highlight Ratio
p12 <- ggplot(plot_data, aes(x = num_major, y = log_keystrokes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, col = "firebrick1") +
  theme_minimal() + labs(title = "Keystrokes vs Major Errors", x = "Num of Minor Errors", y = "log(Keystrokes)")

# 3. Time vs Num Characters
p13 <- ggplot(plot_data, aes(x = num_minor, y = log_time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, col = "firebrick1") +
  theme_minimal() + labs(title = "Time vs Minor Errors", x = "Num of Minor Errors", y = "log(Time)")

# 4. Keystrokes vs Num Characters
p14 <- ggplot(plot_data, aes(x = num_minor, y = log_keystrokes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, col = "firebrick1") +
  theme_minimal() + labs(title = "Keystrokes vs Minor Errors", x = "Num of Minor Errors", y = "log(Keystrokes)")

(p11 | p12) / (p13 | p14) + plot_layout(guides = 'collect')
