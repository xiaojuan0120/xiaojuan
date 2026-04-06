# Load required packages
library(readxl)
library(openxlsx)
library(xgboost)
library(ggplot2)
library(patchwork)
library(dplyr)  # Load dplyr for bind_rows()

# ----------------- Part 1: Data Import -----------------
# Read data file
design <- read.csv("bnti abundant.csv")

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

# ----------------- Part 3: Define Grid of Parameters -----------------
# Define the parameter grid for tuning
param_grid <- expand.grid(
  max_depth = c(3, 6, 9),           # Max depth of trees
  eta = c(0.01, 0.05, 0.1),        # Learning rate
  subsample = c(0.6, 0.8, 1.0),     # Fraction of training data to use
  colsample_bytree = c(0.6, 0.8, 1.0),  # Fraction of features to sample per tree
  lambda = c(0, 1, 2),              # L2 regularization
  alpha = c(0, 0.1, 0.5)            # L1 regularization
)

# ----------------- Part 4: Grid Search -----------------
# Initialize an empty data frame to store results
results <- data.frame(
  max_depth = numeric(),
  eta = numeric(),
  subsample = numeric(),
  colsample_bytree = numeric(),
  lambda = numeric(),
  alpha = numeric(),
  train_r2 = numeric(),
  test_r2 = numeric(),
  stringsAsFactors = FALSE  # Ensure no factor columns
)

# Loop over each parameter combination in the grid
for (i in 1:nrow(param_grid)) {
  # Extract parameters for the current combination
  params <- param_grid[i, ]
  
  # ----------------- Part 5: Train XGBoost Model -----------------
  # Convert data to DMatrix format
  dtrain <- xgb.DMatrix(
    data = as.matrix(train_data[, features]),
    label = train_data$K
  )
  
  dtest <- xgb.DMatrix(
    data = as.matrix(test_data[, features]),
    label = test_data$K
  )
  
  # Train XGBoost model
  model <- xgboost(
    data = dtrain,
    max_depth = params$max_depth,
    eta = params$eta,
    nthread = 4,
    nrounds = 100,
    objective = "reg:squarederror",
    subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    lambda = params$lambda,
    alpha = params$alpha
  )
  
  # ----------------- Part 6: Model Evaluation -----------------
  # Make predictions on training set
  train_pred <- predict(model, as.matrix(train_data[, features]))
  
  # Calculate R² for training set
  train_r2 <- 1 - sum((train_data$K - train_pred)^2) / sum((train_data$K - mean(train_data$K))^2)
  
  # Make predictions on test set
  test_pred <- predict(model, as.matrix(test_data[, features]))
  
  # Calculate R² for test set
  test_r2 <- 1 - sum((test_data$K - test_pred)^2) / sum((test_data$K - mean(test_data$K))^2)
  
  # Store the results in a new data frame
  temp_results <- data.frame(
    max_depth = params$max_depth,
    eta = params$eta,
    subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    lambda = params$lambda,
    alpha = params$alpha,
    train_r2 = train_r2,
    test_r2 = test_r2,
    stringsAsFactors = FALSE  # Ensure no factor columns
  )
  
  # Use bind_rows() to add results safely
  results <- bind_rows(results, temp_results)
}

# ----------------- Part 7: Output Results -----------------
# Print the results
print(results)

# Optionally, save results to a CSV file
write.csv(results, "xgboost.csv", row.names = FALSE)




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
  max_depth = 9,
  eta = 0.1,
  nthread = 4,
  nrounds = 100,
  objective = "reg:squarederror",
  subsample = 1,
  colsample_bytree = 1,
  lambda = 0,
  alpha = 0
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
ggsave("bnti rare.pdf", final_plot, width = 20, height = 8, units = "cm")

# Print completion message
print("Analysis completed successfully!")


# ----------------- Part 1: Load Necessary Libraries -----------------
library(readxl)
library(openxlsx)
library(xgboost)
library(ggplot2)
library(caret)

# ----------------- Part 2: Data Import -----------------
# Read data file
design <- read.csv("bnti rare.csv")

# Extract first 8 columns as features and add target variable K
a <- design[, 2:8]
a$K <- design$K

# Check data structure
str(a)

# ----------------- Part 3: Data Splitting -----------------
set.seed(12345)
train_index <- sample(seq_len(nrow(a)), size = floor(0.7 * nrow(a)))
train_data <- a[train_index, ]
test_data  <- a[-train_index, ]

features <- setdiff(colnames(a), "K")

# ----------------- Part 4: Manual 5-Fold CV -----------------
set.seed(12345)
folds <- createFolds(train_data$K, k = 5, list = TRUE, returnTrain = FALSE)

params <- list(
  max_depth = 9,
  eta = 0.1,
  nthread = 4,
  objective = "reg:squarederror",
  subsample = 1,
  colsample_bytree = 1,
  lambda = 0,
  alpha = 0
)

fold_results <- data.frame(
  Fold = integer(),
  Best_nrounds = integer(),
  RMSE = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

oof_pred <- rep(NA_real_, nrow(train_data))  # out-of-fold prediction

for (i in seq_along(folds)) {
  cat("Processing Fold", i, "...\n")
  
  valid_idx <- folds[[i]]
  train_idx_fold <- setdiff(seq_len(nrow(train_data)), valid_idx)
  
  fold_train <- train_data[train_idx_fold, ]
  fold_valid <- train_data[valid_idx, ]
  
  dtrain_fold <- xgb.DMatrix(
    data = as.matrix(fold_train[, features]),
    label = fold_train$K
  )
  
  dvalid_fold <- xgb.DMatrix(
    data = as.matrix(fold_valid[, features]),
    label = fold_valid$K
  )
  
  watchlist <- list(train = dtrain_fold, eval = dvalid_fold)
  
  model_fold <- xgb.train(
    params = params,
    data = dtrain_fold,
    nrounds = 1000,
    watchlist = watchlist,
    eval_metric = "rmse",
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  pred_valid <- predict(model_fold, dvalid_fold)
  obs_valid  <- fold_valid$K
  
  rmse_fold <- sqrt(mean((obs_valid - pred_valid)^2))
  r2_fold   <- 1 - sum((obs_valid - pred_valid)^2) / sum((obs_valid - mean(obs_valid))^2)
  
  oof_pred[valid_idx] <- pred_valid
  
  fold_results <- rbind(
    fold_results,
    data.frame(
      Fold = i,
      Best_nrounds = model_fold$best_iteration,
      RMSE = rmse_fold,
      R2 = r2_fold
    )
  )
}

# ----------------- Part 5: CV Summary -----------------
mean_rmse <- mean(fold_results$RMSE)
sd_rmse   <- sd(fold_results$RMSE)

mean_r2 <- mean(fold_results$R2)
sd_r2   <- sd(fold_results$R2)

# 整体 OOF R²（推荐论文里报告这个）
oof_r2 <- 1 - sum((train_data$K - oof_pred)^2) / sum((train_data$K - mean(train_data$K))^2)

cat("\n===== 5-Fold CV Results =====\n")
print(fold_results)

cat("\nMean RMSE =", round(mean_rmse, 4), "±", round(sd_rmse, 4), "\n")
cat("Mean R²   =", round(mean_r2, 4), "±", round(sd_r2, 4), "\n")
cat("OOF R²    =", round(oof_r2, 4), "\n")

# ----------------- Part 6: Train Final Model -----------------
dtrain_final <- xgb.DMatrix(
  data = as.matrix(train_data[, features]),
  label = train_data$K
)

dtest <- xgb.DMatrix(
  data = as.matrix(test_data[, features]),
  label = test_data$K
)

final_model <- xgb.train(
  params = params,
  data = dtrain_final,
  nrounds = round(mean(fold_results$Best_nrounds)),
  verbose = 0
)

test_pred <- predict(final_model, dtest)

test_rmse <- sqrt(mean((test_data$K - test_pred)^2))
test_r2   <- 1 - sum((test_data$K - test_pred)^2) / sum((test_data$K - mean(test_data$K))^2)

cat("\n===== Independent Test Set Results =====\n")
cat("Test RMSE =", round(test_rmse, 4), "\n")
cat("Test R²   =", round(test_r2, 4), "\n")

# ----------------- Part 7: Export Results -----------------
write.xlsx(
  list(
    Fold_Results = fold_results,
    Summary = data.frame(
      Mean_RMSE = mean_rmse,
      SD_RMSE   = sd_rmse,
      Mean_R2   = mean_r2,
      SD_R2     = sd_r2,
      OOF_R2    = oof_r2,
      Test_RMSE = test_rmse,
      Test_R2   = test_r2
    )
  ),
  file = "zxj稀有results.xlsx",
  rowNames = FALSE
)

# ======================== 学习曲线绘制（直接加在这里） ========================
# 提取每一轮的交叉验证误差，绘制 XGBoost 学习曲线
library(dplyr)
library(ggplot2)

# 获取所有折的迭代日志
all_iter_logs <- list()

for (i in seq_along(folds)) {
  valid_idx <- folds[[i]]
  train_idx_fold <- setdiff(seq_len(nrow(train_data)), valid_idx)
  fold_train <- train_data[train_idx_fold, ]
  fold_valid <- train_data[valid_idx, ]
  
  dtrain_fold <- xgb.DMatrix(data = as.matrix(fold_train[, features]), label = fold_train$K)
  dvalid_fold <- xgb.DMatrix(data = as.matrix(fold_valid[, features]), label = fold_valid$K)
  watchlist <- list(train = dtrain_fold, eval = dvalid_fold)
  
  model_fold <- xgb.train(
    params = params,
    data = dtrain_fold,
    nrounds = 1000,
    watchlist = watchlist,
    eval_metric = "rmse",
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  iter_log <- model_fold$evaluation_log
  iter_log$fold <- i
  all_iter_logs[[i]] <- iter_log
}

# 合并所有迭代日志
curve_data <- bind_rows(all_iter_logs)

# 计算每一迭代的平均RMSE（5折平均）
learning_curve <- curve_data %>%
  group_by(iter) %>%
  summarise(
    mean_train_rmse = mean(train_rmse, na.rm = TRUE),
    mean_eval_rmse = mean(eval_rmse, na.rm = TRUE),
    .groups = "drop"
  )

# 找到最优迭代次数
best_iter <- which.min(learning_curve$mean_eval_rmse)
best_rmse <- learning_curve$mean_eval_rmse[best_iter]

# 绘制学习曲线
p_learning <- ggplot(learning_curve, aes(x = iter)) +
  geom_line(aes(y = mean_train_rmse, color = "Training RMSE"), linewidth = 0.8) +
  geom_line(aes(y = mean_eval_rmse, color = "CV RMSE"), linewidth = 0.8) +
  geom_vline(xintercept = best_iter, color = "#E67E22", linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = best_iter + 30, y = best_rmse, 
           label = paste("Optimal =", best_iter), color = "#E67E22", fontface = "bold", size = 4.5) +
  scale_color_manual(values = c("Training RMSE" = "#3498DB", "CV RMSE" = "#E74C3C")) +
  labs(x = "Number of Trees (Iterations)",
       y = "RMSE",
       color = "") +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )

print(p_learning)

# 保存图片（可直接放补充材料）
ggsave("zxj稀有XGBoost_Learning_Curve.pdf", p_learning, width = 9, height = 6, dpi = 300)

