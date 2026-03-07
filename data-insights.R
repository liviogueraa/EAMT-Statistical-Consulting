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



##### Categorical Variables #####

data <- read.csv("data/data.csv")
data_categorical <- data[,-c(7,8,9,10,11,12,13)]

# factorizing and summarising the variables
data_categorical[] <- lapply(data_categorical, as.factor)
sapply(data_categorical, unique, simplify = FALSE)

# Creating tables per variable
# The corrected error "tags_edited" is removed, for simplicity of visualization

lapply(data_categorical[,-7], table)