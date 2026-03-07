############################################################################
# ----- THIS SECTION IS DEDICATED TO A PRIOR ANALYSIS OF THE DATASET ----- #
############################################################################

# Required Packages
library(readr)
library(ggplot2)
library(dplyr)

#=======
library(see)
library(patchwork)

# Loading dataset
data <- read_csv("data/data.csv")

#=======
# Tables  
table(data$condition)
table(data$domain)

# Figures
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

