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
library(FactoMineR)  # For PCA
library(glmnet)     # For Lasso
library(pROC)       # For ROC (optional, included for consistency)

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

# Evaluation function (updated to include training precision)
evaluate_model <- function(pred_train, pred_val, pred_test, time_train, time_infer, y_train, y_val, y_test) {
  error <- function(y, pred) mean(y != pred)
  accuracy <- function(y, pred) mean(y == pred)
  return(data.frame(
    `Erreur entraînement` = round(error(y_train, pred_train), 3),
    `Erreur validation` = round(error(y_val, pred_val), 3),
    `Erreur test` = round(error(y_test, pred_test), 3),
    `Précision entraînement` = round(accuracy(y_train, pred_train), 3),
    `Précision validation` = round(accuracy(y_val, pred_val), 3),  # ← ajout ici
    `Précision test` = round(accuracy(y_test, pred_test), 3),
    `Temps entraînement (ms)` = round(time_train * 1000, 1),
    `Temps inférence (ms)` = round(time_infer * 1000, 1)
  ))
}


results <- list()

# 1. Logistic Regression (Complete)
tic()
logreg_model <- glm(Class ~ ., data = train, family = binomial)
t_train <- toc(log = TRUE)
tic()
logreg_pred_train <- predict(logreg_model, train, type = "response")
logreg_pred_val <- predict(logreg_model, val, type = "response")
logreg_pred_test <- predict(logreg_model, test, type = "response")
logreg_pred_train <- factor(ifelse(logreg_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
logreg_pred_val <- factor(ifelse(logreg_pred_val > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
logreg_pred_test <- factor(ifelse(logreg_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression"]] <- evaluate_model(logreg_pred_train, logreg_pred_val, logreg_pred_test,
                                                   t_train$toc - t_train$tic,
                                                   t_inf$toc - t_inf$tic,
                                                   y_train, y_val, y_test)

# 2. Logistic Regression with PCA
tic()
pca_model <- PCA(X_train, scale.unit = TRUE, graph = FALSE)
train_pca <- pca_model$ind$coord[, 1:2]
val_pca <- predict(pca_model, newdata = X_val)$coord[, 1:2]
test_pca <- predict(pca_model, newdata = X_test)$coord[, 1:2]
train_pca_data <- data.frame(Dim.1 = train_pca[, 1], Dim.2 = train_pca[, 2], Class = y_train)
pca_logreg_model <- glm(Class ~ Dim.1 + Dim.2, data = train_pca_data, family = binomial)
t_train <- toc(log = TRUE)
tic()
val_pca_data <- data.frame(Dim.1 = val_pca[, 1], Dim.2 = val_pca[, 2])
test_pca_data <- data.frame(Dim.1 = test_pca[, 1], Dim.2 = test_pca[, 2])
pca_pred_train <- predict(pca_logreg_model, train_pca_data, type = "response")
pca_pred_val <- predict(pca_logreg_model, val_pca_data, type = "response")
pca_pred_test <- predict(pca_logreg_model, test_pca_data, type = "response")
pca_pred_train <- factor(ifelse(pca_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
pca_pred_val <- factor(ifelse(pca_pred_val > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
pca_pred_test <- factor(ifelse(pca_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression PCA"]] <- evaluate_model(pca_pred_train, pca_pred_val, pca_pred_test,
                                                       t_train$toc - t_train$tic,
                                                       t_inf$toc - t_inf$tic,
                                                       y_train, y_val, y_test)

# 3. Logistic Regression with AIC
tic()
logreg_aic_model <- step(logreg_model, direction = "both", trace = FALSE)
t_train <- toc(log = TRUE)
tic()
aic_pred_train <- predict(logreg_aic_model, train, type = "response")
aic_pred_val <- predict(logreg_aic_model, val, type = "response")
aic_pred_test <- predict(logreg_aic_model, test, type = "response")
aic_pred_train <- factor(ifelse(aic_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
aic_pred_val <- factor(ifelse(aic_pred_val > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
aic_pred_test <- factor(ifelse(aic_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression AIC"]] <- evaluate_model(aic_pred_train, aic_pred_val, aic_pred_test,
                                                       t_train$toc - t_train$tic,
                                                       t_inf$toc - t_inf$tic,
                                                       y_train, y_val, y_test)

# 4. Lasso Logistic Regression
tic()
lasso_model <- cv.glmnet(as.matrix(X_train), as.numeric(y_train) - 1, family = "binomial", alpha = 1)
lasso_fit <- glmnet(as.matrix(X_train), as.numeric(y_train) - 1, family = "binomial", alpha = 1, lambda = lasso_model$lambda.min)
t_train <- toc(log = TRUE)
tic()
lasso_pred_train <- predict(lasso_fit, newx = as.matrix(X_train), type = "response")
lasso_pred_val <- predict(lasso_fit, newx = as.matrix(X_val), type = "response")
lasso_pred_test <- predict(lasso_fit, newx = as.matrix(X_test), type = "response")
lasso_pred_train <- factor(ifelse(lasso_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
lasso_pred_val <- factor(ifelse(lasso_pred_val > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
lasso_pred_test <- factor(ifelse(lasso_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Lasso Logistic Regression"]] <- evaluate_model(lasso_pred_train, lasso_pred_val, lasso_pred_test,
                                                         t_train$toc - t_train$tic,
                                                         t_inf$toc - t_inf$tic,
                                                         y_train, y_val, y_test)

# 5. SVM Linear
tic()
svm_linear_model <- svm(Class ~ ., data = train, kernel = "linear", probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_linear_pred_train <- predict(svm_linear_model, train)
svm_linear_pred_val <- predict(svm_linear_model, val)
svm_linear_pred_test <- predict(svm_linear_model, test)
t_inf <- toc(log = TRUE)
results[["SVM Linear"]] <- evaluate_model(svm_linear_pred_train, svm_linear_pred_val, svm_linear_pred_test,
                                          t_train$toc - t_train$tic,
                                          t_inf$toc - t_inf$tic,
                                          y_train, y_val, y_test)

# 6. SVM Polynomial
tic()
svm_poly_model <- svm(Class ~ ., data = train, kernel = "polynomial", degree = 3, probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_poly_pred_train <- predict(svm_poly_model, train)
svm_poly_pred_val <- predict(svm_poly_model, val)
svm_poly_pred_test <- predict(svm_poly_model, test)
t_inf <- toc(log = TRUE)
results[["SVM Polynomial"]] <- evaluate_model(svm_poly_pred_train, svm_poly_pred_val, svm_poly_pred_test,
                                              t_train$toc - t_train$tic,
                                              t_inf$toc - t_inf$tic,
                                              y_train, y_val, y_test)

# 7. Decision Tree
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

# 8. Random Forest
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

# 9. Gradient Boosting (XGBoost)
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

# 10. Naive Bayes
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

# 11. k-NN
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

# 12. Neural Network
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
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, position = "center")