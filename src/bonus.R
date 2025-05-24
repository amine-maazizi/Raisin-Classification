# Clear workspace and close graphics
rm(list = objects())
graphics.off()

# Load libraries
library(openxlsx)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(xgboost)
library(class)
library(nnet)
library(kableExtra)
library(tictoc)

# Load data
data <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)
data$Class <- as.factor(data$Class)

# Normalize features
data_scaled <- data
data_scaled[,-ncol(data)] <- scale(data[,-ncol(data)])

set.seed(123)

# Split dataset: 70% train, 15% val, 15% test
n <- nrow(data_scaled)
train_idx <- sample(seq_len(n), size = 0.7 * n)
remaining_idx <- setdiff(seq_len(n), train_idx)
val_idx <- sample(remaining_idx, size = 0.5 * length(remaining_idx))
test_idx <- setdiff(remaining_idx, val_idx)

train <- data_scaled[train_idx, ]
val <- data_scaled[val_idx, ]
test <- data_scaled[test_idx, ]

X_train <- train[,-ncol(train)]
y_train <- train$Class
X_val <- val[,-ncol(val)]
y_val <- val$Class
X_test <- test[,-ncol(test)]
y_test <- test$Class

# Evaluation function
evaluate_model <- function(pred_train, pred_val, pred_test, time_train, time_infer, y_train, y_val, y_test) {
  error <- function(y, pred) mean(y != pred)
  accuracy <- function(y, pred) mean(y == pred)
  return(data.frame(
    `Erreur entraînement` = round(error(y_train, pred_train), 3),
    `Erreur validation` = round(error(y_val, pred_val), 3),
    `Erreur test` = round(error(y_test, pred_test), 3),
    `Précision` = round(accuracy(y_test, pred_test), 3),
    `Temps entraînement` = round(time_train, 3),
    `Temps inférence` = round(time_infer, 3)
  ))
}

results <- list()

# 1. Decision Tree
tic()
tree_model <- rpart(Class ~ ., data = train)
t_train <- toc(log = TRUE)
tic()
tree_pred_train <- predict(tree_model, train, type = "class")
tree_pred_val <- predict(tree_model, val, type = "class")
tree_pred_test <- predict(tree_model, test, type = "class")
t_inf <- toc(log = TRUE)
results[["Arbre de décision"]] <- evaluate_model(tree_pred_train, tree_pred_val, tree_pred_test,
                                                 t_train$toc - t_train$tic,
                                                 t_inf$toc - t_inf$tic,
                                                 y_train, y_val, y_test)

# 2. Random Forest
tic()
rf_model <- randomForest(Class ~ ., data = train)
t_train <- toc(log = TRUE)
tic()
rf_pred_train <- predict(rf_model, train)
rf_pred_val <- predict(rf_model, val)
rf_pred_test <- predict(rf_model, test)
t_inf <- toc(log = TRUE)
results[["Random Forest"]] <- evaluate_model(rf_pred_train, rf_pred_val, rf_pred_test,
                                             t_train$toc - t_train$tic,
                                             t_inf$toc - t_inf$tic,
                                             y_train, y_val, y_test)

# 3. Gradient Boosting (XGBoost)
train_x <- as.matrix(X_train)
val_x <- as.matrix(X_val)
test_x <- as.matrix(X_test)
train_y <- as.numeric(y_train) - 1
val_y <- as.numeric(y_val) - 1
test_y <- as.numeric(y_test) - 1

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dval <- xgb.DMatrix(data = val_x)
dtest <- xgb.DMatrix(data = test_x)

tic()
xgb_model <- xgboost(data = dtrain, nrounds = 50, objective = "binary:logistic", verbose = 0)
t_train <- toc(log = TRUE)
tic()
xgb_pred_train <- as.numeric(predict(xgb_model, dtrain) > 0.5)
xgb_pred_val <- as.numeric(predict(xgb_model, dval) > 0.5)
xgb_pred_test <- as.numeric(predict(xgb_model, dtest) > 0.5)
t_inf <- toc(log = TRUE)

decode <- function(pred) factor(ifelse(pred == 1, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
results[["Gradient Boosting"]] <- evaluate_model(decode(xgb_pred_train), decode(xgb_pred_val), decode(xgb_pred_test),
                                                 t_train$toc - t_train$tic,
                                                 t_inf$toc - t_inf$tic,
                                                 y_train, y_val, y_test)

# 4. Naive Bayes
tic()
nb_model <- naiveBayes(Class ~ ., data = train)
t_train <- toc(log = TRUE)
tic()
nb_pred_train <- predict(nb_model, train)
nb_pred_val <- predict(nb_model, val)
nb_pred_test <- predict(nb_model, test)
t_inf <- toc(log = TRUE)
results[["Naive Bayes"]] <- evaluate_model(nb_pred_train, nb_pred_val, nb_pred_test,
                                           t_train$toc - t_train$tic,
                                           t_inf$toc - t_inf$tic,
                                           y_train, y_val, y_test)

# 5. k-NN
k <- 5
tic()
knn_pred_train <- knn(train = X_train, test = X_train, cl = y_train, k = k)
knn_pred_val <- knn(train = X_train, test = X_val, cl = y_train, k = k)
knn_pred_test <- knn(train = X_train, test = X_test, cl = y_train, k = k)
t_train <- list(tic = 0, toc = 0)  # no training phase
t_inf <- toc(log = TRUE)
results[["k-NN"]] <- evaluate_model(knn_pred_train, knn_pred_val, knn_pred_test,
                                    0,  # no training
                                    t_inf$toc - t_inf$tic,
                                    y_train, y_val, y_test)

# 6. Neural Network
tic()
nn_model <- nnet(Class ~ ., data = train, size = 5, maxit = 200, trace = FALSE)
t_train <- toc(log = TRUE)
tic()
nn_pred_train <- predict(nn_model, train, type = "class")
nn_pred_val <- predict(nn_model, val, type = "class")
nn_pred_test <- predict(nn_model, test, type = "class")
t_inf <- toc(log = TRUE)
results[["Réseau de neurones"]] <- evaluate_model(nn_pred_train, nn_pred_val, nn_pred_test,
                                                  t_train$toc - t_train$tic,
                                                  t_inf$toc - t_inf$tic,
                                                  y_train, y_val, y_test)

# Print results
results_df <- do.call(rbind, results)
results_df <- cbind(Modèle = rownames(results_df), results_df)
rownames(results_df) <- NULL

# Pretty table
library(knitr)
library(kableExtra)
kable(results_df, format = "html", digits = 3, booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE)
