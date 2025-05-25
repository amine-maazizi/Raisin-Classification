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
library(FactoMineR)
library(glmnet)
library(pROC)

# Load data
data <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)
data$Class <- as.factor(data$Class)

# Normalize features
data_scaled <- data
data_scaled[,-ncol(data)] <- scale(data[,-ncol(data)])

set.seed(123)

# 90/10 train/test split
n <- nrow(data_scaled)
test_idx <- sample(seq_len(n), size = 0.1 * n)
train_idx <- setdiff(seq_len(n), test_idx)
train <- data_scaled[train_idx, ]
test <- data_scaled[test_idx, ]

X_train <- train[,-ncol(train)]
y_train <- train$Class
X_test <- test[,-ncol(test)]
y_test <- test$Class

# Evaluation function
evaluate_model <- function(pred_train, pred_test, time_train, time_infer, y_train, y_test) {
  error <- function(y, pred) mean(y != pred)
  accuracy <- function(y, pred) mean(y == pred)
  return(data.frame(
    `Erreur entraînement` = round(error(y_train, pred_train), 3),
    `Erreur test` = round(error(y_test, pred_test), 3),
    `Précision entraînement` = round(accuracy(y_train, pred_train), 3),
    `Précision test` = round(accuracy(y_test, pred_test), 3),
    `Temps entraînement (ms)` = round(time_train * 1000, 1),
    `Temps inférence (ms)` = round(time_infer * 1000, 1)
  ))
}

results <- list()

# 1. Logistic Regression
tic()
logreg_model <- glm(Class ~ ., data = train, family = binomial)
t_train <- toc(log = TRUE)
tic()
logreg_pred_train <- predict(logreg_model, train, type = "response")
logreg_pred_test <- predict(logreg_model, test, type = "response")
logreg_pred_train <- factor(ifelse(logreg_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
logreg_pred_test <- factor(ifelse(logreg_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression"]] <- evaluate_model(logreg_pred_train, logreg_pred_test,
                                                   t_train$toc - t_train$tic,
                                                   t_inf$toc - t_inf$tic,
                                                   y_train, y_test)

# 2. Logistic Regression with PCA (2 components)
tic()
pca_model <- PCA(X_train, scale.unit = TRUE, graph = FALSE)
train_pca <- pca_model$ind$coord[, 1:2]
test_pca <- predict(pca_model, newdata = X_test)$coord[, 1:2]
train_pca_data <- data.frame(Dim.1 = train_pca[, 1], Dim.2 = train_pca[, 2], Class = y_train)
pca_logreg_model <- glm(Class ~ Dim.1 + Dim.2, data = train_pca_data, family = binomial)
t_train <- toc(log = TRUE)
tic()
test_pca_data <- data.frame(Dim.1 = test_pca[, 1], Dim.2 = test_pca[, 2])
pca_pred_train <- predict(pca_logreg_model, train_pca_data, type = "response")
pca_pred_test <- predict(pca_logreg_model, test_pca_data, type = "response")
pca_pred_train <- factor(ifelse(pca_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
pca_pred_test <- factor(ifelse(pca_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression PCA"]] <- evaluate_model(pca_pred_train, pca_pred_test,
                                                       t_train$toc - t_train$tic,
                                                       t_inf$toc - t_inf$tic,
                                                       y_train, y_test)

# 3. Logistic Regression AIC
tic()
logreg_aic_model <- step(logreg_model, direction = "both", trace = FALSE)
t_train <- toc(log = TRUE)
tic()
aic_pred_train <- predict(logreg_aic_model, train, type = "response")
aic_pred_test <- predict(logreg_aic_model, test, type = "response")
aic_pred_train <- factor(ifelse(aic_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
aic_pred_test <- factor(ifelse(aic_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression AIC"]] <- evaluate_model(aic_pred_train, aic_pred_test,
                                                       t_train$toc - t_train$tic,
                                                       t_inf$toc - t_inf$tic,
                                                       y_train, y_test)

# 4. Lasso Logistic Regression (with 10-fold CV)
tic()
lasso_model <- cv.glmnet(as.matrix(X_train), as.numeric(y_train) - 1, family = "binomial", alpha = 1, nfolds = 10)
lasso_fit <- glmnet(as.matrix(X_train), as.numeric(y_train) - 1, family = "binomial", alpha = 1, lambda = lasso_model$lambda.min)
t_train <- toc(log = TRUE)
tic()
lasso_pred_train <- predict(lasso_fit, newx = as.matrix(X_train), type = "response")
lasso_pred_test <- predict(lasso_fit, newx = as.matrix(X_test), type = "response")
lasso_pred_train <- factor(ifelse(lasso_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
lasso_pred_test <- factor(ifelse(lasso_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Lasso Logistic Regression"]] <- evaluate_model(lasso_pred_train, lasso_pred_test,
                                                         t_train$toc - t_train$tic,
                                                         t_inf$toc - t_inf$tic,
                                                         y_train, y_test)

# 5. Linear SVM
tic()
svm_linear_model <- svm(Class ~ ., data = train, kernel = "linear", probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_linear_pred_train <- predict(svm_linear_model, X_train)
svm_linear_pred_test <- predict(svm_linear_model, X_test)
t_inf <- toc(log = TRUE)
results[["Linear SVM"]] <- evaluate_model(svm_linear_pred_train, svm_linear_pred_test,
                                          t_train$toc - t_train$tic,
                                          t_inf$toc - t_inf$tic,
                                          y_train, y_test)

# 6. Polynomial SVM
tic()
svm_poly_model <- svm(Class ~ ., data = train, kernel = "polynomial", degree = 3, probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_poly_pred_train <- predict(svm_poly_model, X_train)
svm_poly_pred_test <- predict(svm_poly_model, X_test)
t_inf <- toc(log = TRUE)
results[["Polynomial SVM"]] <- evaluate_model(svm_poly_pred_train, svm_poly_pred_test,
                                              t_train$toc - t_train$tic,
                                              t_inf$toc - t_inf$tic,
                                              y_train, y_test)

# 7. Decision Tree
tic()
dt_model <- rpart(Class ~ ., data = train, method = "class")
t_train <- toc(log = TRUE)
tic()
dt_pred_train <- predict(dt_model, X_train, type = "class")
dt_pred_test <- predict(dt_model, X_test, type = "class")
t_inf <- toc(log = TRUE)
results[["Decision Tree"]] <- evaluate_model(dt_pred_train, dt_pred_test,
                                             t_train$toc - t_train$tic,
                                             t_inf$toc - t_inf$tic,
                                             y_train, y_test)

# 8. Random Forest
tic()
rf_model <- randomForest(Class ~ ., data = train, ntree = 100)
t_train <- toc(log = TRUE)
tic()
rf_pred_train <- predict(rf_model, X_train)
rf_pred_test <- predict(rf_model, X_test)
t_inf <- toc(log = TRUE)
results[["Random Forest"]] <- evaluate_model(rf_pred_train, rf_pred_test,
                                             t_train$toc - t_train$tic,
                                             t_inf$toc - t_inf$tic,
                                             y_train, y_test)

# 9. Gradient Boosting (XGBoost)
tic()
xgb_data_train <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
xgb_data_test <- xgb.DMatrix(data = as.matrix(X_test), label = as.numeric(y_test) - 1)
xgb_model <- xgboost(data = xgb_data_train, nrounds = 100, objective = "binary:logistic", verbose = 0)
t_train <- toc(log = TRUE)
tic()
xgb_pred_train <- predict(xgb_model, xgb_data_train)
xgb_pred_test <- predict(xgb_model, xgb_data_test)
xgb_pred_train <- factor(ifelse(xgb_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
xgb_pred_test <- factor(ifelse(xgb_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Gradient Boosting"]] <- evaluate_model(xgb_pred_train, xgb_pred_test,
                                                 t_train$toc - t_train$tic,
                                                 t_inf$toc - t_inf$tic,
                                                 y_train, y_test)

# 10. Naive Bayes
tic()
nb_model <- naiveBayes(Class ~ ., data = train)
t_train <- toc(log = TRUE)
tic()
nb_pred_train <- predict(nb_model, X_train)
nb_pred_test <- predict(nb_model, X_test)
t_inf <- toc(log = TRUE)
results[["Naive Bayes"]] <- evaluate_model(nb_pred_train, nb_pred_test,
                                           t_train$toc - t_train$tic,
                                           t_inf$toc - t_inf$tic,
                                           y_train, y_test)

# 11. k-Nearest Neighbors (k-NN)
tic()
knn_pred_train <- knn(train = X_train, test = X_train, cl = y_train, k = 5)
t_train <- toc(log = TRUE)
tic()
knn_pred_test <- knn(train = X_train, test = X_test, cl = y_train, k = 5)
t_inf <- toc(log = TRUE)
results[["k-NN"]] <- evaluate_model(knn_pred_train, knn_pred_test,
                                    t_train$toc - t_train$tic,
                                    t_inf$toc - t_inf$tic,
                                    y_train, y_test)

# 12. Neural Network
tic()
nn_model <- nnet(Class ~ ., data = train, size = 4, decay = 0.0001, maxit = 500, trace = FALSE)
t_train <- toc(log = TRUE)
tic()
nn_pred_train <- predict(nn_model, X_train, type = "class")
nn_pred_test <- predict(nn_model, X_test, type = "class")
nn_pred_train <- factor(nn_pred_train, levels = levels(y_train))
nn_pred_test <- factor(nn_pred_test, levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Neural Network"]] <- evaluate_model(nn_pred_train, nn_pred_test,
                                              t_train$toc - t_train$tic,
                                              t_inf$toc - t_inf$tic,
                                              y_train, y_test)

# Combine results
results_df <- do.call(rbind, results)
results_df <- cbind(Modèle = rownames(results_df), results_df)
rownames(results_df) <- NULL

# Pretty table
kable(results_df, format = "html", digits = 3, booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, position = "center")