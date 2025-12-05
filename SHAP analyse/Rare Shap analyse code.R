# Load required packages
library(readxl)
library(openxlsx)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)

# ----------------- Part 1: Data Import -----------------
# Read data file
design <- read.csv("bnti rare.csv")

# Extract first 8 columns as features and add target variable K
a <- design[, 2:8]  # Extract first 8 columns as features
a$K <- design$K  # Add K column as target variable

# Check data structure
str(a)

# ----------------- Part 2: Data Splitting -----------------
# Set random seed and split training/test sets
set.seed(12345)
train_index <- sample(1:nrow(a), nrow(a) * 0.7)  # 70% training, 30% testing
train_data <- a[train_index, ]
test_data <- a[-train_index, ]

# Get feature column names (excluding K)
features <- colnames(a)[colnames(a) != "K"]

# ----------------- Part 3: XGBoost Regression Model -----------------
# Convert data to DMatrix format
dtrain <- xgb.DMatrix(
  data = as.matrix(train_data[, features]),
  label = train_data$K
)

dtest <- xgb.DMatrix(
  data = as.matrix(test_data[, features]),
  label = test_data$K
)

# Train XGBoost regression model
train_xgboost <- xgboost(
  data = dtrain,
  max_depth = 4,
  eta = 0.1,
  nthread = 4,
  nrounds = 100,
  objective = "reg:squarederror",
  subsample = 0.8,
  colsample_bytree = 0.8,
  lambda = 1,
  alpha = 0.5
)

# ----------------- Part 4: Training Set Performance Evaluation -----------------
# Make predictions on training set
train_pred <- predict(
  train_xgboost, 
  as.matrix(train_data[, features])
)

# Calculate R² for training set
train_r2 <- 1 - sum((train_data$K - train_pred)^2) / sum((train_data$K - mean(train_data$K))^2)
print(paste("Training R²:", train_r2))

# ----------------- Part 5: Test Set Model Evaluation -----------------
# Make predictions on test set
test_pred <- predict(
  train_xgboost, 
  as.matrix(test_data[, features])
)

# Calculate R² for test set
test_r2 <- 1 - sum((test_data$K - test_pred)^2) / sum((test_data$K - mean(test_data$K))^2)
print(paste("Test R²:", test_r2))

# ----------------- Part 6: SHAP Value Calculation and Visualization -----------------
# Calculate SHAP values
shap_train_xgboost <- shapviz(train_xgboost, X_pred = as.matrix(train_data[, features]))

# Plot feature importance (bar chart)
p1 <- sv_importance(shap_train_xgboost, fill = "#5878AF") + theme_bw()

# Plot feature importance (beeswarm plot)
p3 <- sv_importance(shap_train_xgboost, kind = "beeswarm") + theme_bw()

# Combine plots
final_plot <- p1 + p3 + plot_layout(ncol = 2)

# Display final plot
print(final_plot)

# Save plot as PDF
ggsave("final_plot_BNTI_rare.pdf", final_plot, width = 20, height = 8, units = "cm")

# Print completion message
print("Analysis completed successfully!")