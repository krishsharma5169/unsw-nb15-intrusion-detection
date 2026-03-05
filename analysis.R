# ================================================================
# Group X - Cyber Intrusion Detection Coursework
# Members:
# Krish, TPXXXXXX
# Member2, TPXXXXXX
# Member3, TPXXXXXX
# Member4, TPXXXXXX
# ================================================================

# ================================================================
# Objective 1: To investigate the relationship between sload & dload
#              towards the presence of attack (label)
# Student: Krish, TPXXXXXX
# ================================================================

# --- Analysis 1-1: Data Import and Exploration ---
# What I want to discover: Inspect dataset structure and identify missing values
data <- read.csv("C:\\Users\\deads\\OneDrive\\Desktop\\APU studies\\SEM 3 (Year 2)\\Programming for Data Analysis\\R project\\UNSW-NB15_uncleaned.csv")

# Ensure key variables are numeric (important for quantile, correlation, etc.)
data$sload <- as.numeric(gsub(",", "", data$sload))
data$dload <- as.numeric(gsub(",", "", data$dload))
data$label <- as.numeric(gsub(",", "", data$label))

# Explore dataset
str(data)
summary(data[, c("sload", "dload", "label")])
head(data, 10)


# --- Analysis 1-2: Missing Value Detection and Imputation ---
# What I want to discover: Handle missing values without removing rows

# Check missing counts
sum(is.na(data$sload))
sum(is.na(data$dload))

# Median Imputation for numeric variables
median_impute <- function(x) { 
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

data$sload <- median_impute(data$sload)
data$dload <- median_impute(data$dload)

# Mode function for categorical (if needed)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Example: if label had missing values, fill with mode
# data$label[is.na(data$label)] <- getmode(data$label)

# Confirm no missing values remain
sum(is.na(data$sload))
sum(is.na(data$dload))


# --- Analysis 1-3: Hot Deck Imputation (Alternative) ---
# What I want to discover: Compare with another valid imputation method
library(VIM)
data_hd <- hotdeck(data, variable = c("sload", "dload"))
sum(is.na(data_hd$sload))
sum(is.na(data_hd$dload))


# --- Analysis 1-4: Outlier Treatment ---
# What I want to discover: Cap extreme outliers using 99th percentile

cap_sload <- quantile(data$sload, 0.99, na.rm = TRUE)
data$sload[data$sload > cap_sload] <- cap_sload

cap_dload <- quantile(data$dload, 0.99, na.rm = TRUE)
data$dload[data$dload > cap_dload] <- cap_dload


# --- Analysis 1-5: Descriptive Statistics ---
# What I want to discover: Summarize cleaned numeric variables
summary(data[, c("sload", "dload")])


# --- Analysis 1-6: Visualization ---
# What I want to discover: Distribution differences between attack (1) and normal (0)

library(ggplot2)

# Histogram
ggplot(data, aes(x = sload, fill = factor(label))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  labs(title = "Histogram of Source Load (sload)", fill = "Label")

# Boxplot
ggplot(data, aes(x = factor(label), y = sload, fill = factor(label))) +
  geom_boxplot() +
  labs(title = "Boxplot of Source Load by Label", x = "Traffic Type", y = "sload")


# --- Analysis 1-7: Correlation Analysis ---
# What I want to discover: Relationship between sload, dload, and attack label
cor_matrix <- cor(data[, c("sload", "dload", "label")], use = "complete.obs")
print(cor_matrix)


# ================================================================
# Extra Feature 1: Correlation Heatmap
# Student: Krish, TPXXXXXX
# ================================================================
# Comment: A heatmap provides a clearer visual representation 
# of the correlation between sload, dload, and label.

library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45)


# ================================================================
# Extra Feature 2: Logistic Regression
# Student: Krish, TPXXXXXX
# ================================================================
# Comment: Logistic regression was applied to evaluate whether 
# sload and dload can significantly predict the presence of attack.

log_model <- glm(label ~ sload + dload, data = data, family = binomial)
summary(log_model)

# Odds Ratios
exp(coef(log_model))


# ================================================================
# Extra Feature 3: Scatter Plot of sload vs dload
# Student: Krish, TPXXXXXX
# ================================================================
# Comment: A scatter plot helps visualize separation between normal 
# and attack traffic based on network load features.

ggplot(data, aes(x = sload, y = dload, color = factor(label))) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of sload vs dload", color = "Label")
