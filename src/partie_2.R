library(openxlsx)
library(FactoMineR)
library(ggplot2)
library(ggrepel)  # pour les labels lisibles

# Charger les données
raisin = read.xlsx("dataset/Raisin.xlsx", sheet = 1)

# Définir l'échantillon d'apprentissage et de test
set.seed(1)
n = nrow(raisin)
train = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(2/3, 1/3))
test = !train

#QUESTION 2
# Séparer les données (variables explicatives uniquement)
raisin_train = raisin[train, 1:7]
raisin_test = raisin[test, 1:7]

# Effectuer l'ACP sur l'échantillon d'apprentissage avec centrage et réduction
raisin_pca = PCA(raisin_train, scale.unit = TRUE, graph = FALSE)

# Projeter les observations du jeu de test sur les axes principaux
raisin_test_proj = predict(raisin_pca, newdata = raisin_test)

# Les projections sur le premier plan principal sont dans raisin_test_proj$coord[,1:2]
raisin_test_proj$coord[,1:2]

#QUESTION 3
raisin$Class = factor(raisin$Class)

## Modèle complet
model_complet = glm(Class ~ ., data = raisin[train, ], family = binomial)


## Modèle avec les deux premières composantes principales
# Ajouter les composantes principales à l'échantillon d'apprentissage
raisin_train_pca = raisin_pca$ind$coord[,1:2]
raisin_train_with_pca = data.frame(raisin_train_pca, Class = raisin[train, "Class"])

model_pca = glm(Class ~ Dim.1 + Dim.2, data = raisin_train_with_pca, family = binomial)


## Modèle avec sélection de variables par AIC
model_aic = step(model_complet, direction = "both")


## Modèle avec régression pénalisée lasso

library(glmnet)

# Préparer les données
X_train = as.matrix(raisin[train, 1:7])
y_train = as.numeric(raisin$Class[train]) - 1

# Régression lasso avec validation croisée pour choisir lambda
cv_lasso = cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)

model_lasso = glmnet(X_train, y_train, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)


#Choix des hyper-paramètres :
  
#  Le modèle lasso nécessite de choisir l'hyper-paramètre lambda, qui contrôle l'intensité de la pénalisation. Nous utilisons cv.glmnet pour effectuer une validation croisée et sélectionner le lambda optimal (cv_lasso$lambda.min), correspondant à l'erreur minimale de validation croisée.
# Les autres modèles (complet, PCA, AIC) n'ont pas d'hyper-paramètres explicites à ajuster dans ce contexte.



# Question 4
library(e1071)

# SVM linéaire
model_svm_linear = svm(Class ~ ., data = raisin[train, ], kernel = "linear", probability = TRUE)

# SVM avec noyau polynomial de degré 3
model_svm_poly <- svm(Class ~ ., data = raisin[train, ], kernel = "polynomial", degree = 3, probability = TRUE)


#Remarque :
#L'option probability = TRUE est activée pour permettre le calcul des probabilités prédites, utile pour les courbes ROC (question 5).
#Les hyper-paramètres comme cost (paramètre C) ou degree pourraient être optimisés via validation croisée, mais ici, nous utilisons les valeurs par défaut pour simplifier.



# Question 5
library(pROC)

# Fonction pour obtenir les probabilités prédites
get_prob <- function(model, data) {
  if (inherits(model, "glm")) {
    return(predict(model, newdata = data, type = "response"))
  } else if (inherits(model, "svm")) {
    return(attr(predict(model, newdata = data, probability = TRUE), "probabilities")[,2])
  } else if (inherits(model, "glmnet")) {
    return(predict(model, newx = as.matrix(data[,1:7]), type = "response")[,1])
  }
}

# Calculer les probabilités
prob_complet_train <- get_prob(model_complet, raisin[train, ])
prob_complet_test <- get_prob(model_complet, raisin[test, ])

# Pour le modèle PCA, utiliser les projections du jeu de test
raisin_test_pca <- raisin_test_proj$coord[,1:2]
prob_pca_test <- predict(model_pca, newdata = data.frame(Dim.1 = raisin_test_pca[,1], Dim.2 = raisin_test_pca[,2]), type = "response")

prob_aic_test <- get_prob(model_aic, raisin[test, ])
prob_lasso_test <- predict(model_lasso, newx = as.matrix(raisin[test, 1:7]), type = "response")[,1]
prob_svm_linear_test <- attr(predict(model_svm_linear, newdata = raisin[test, ], probability = TRUE), "probabilities")[,2]
prob_svm_poly_test <- attr(predict(model_svm_poly, newdata = raisin[test, ], probability = TRUE), "probabilities")[,2]

# Ensure Class is a factor
raisin$Class <- as.factor(raisin$Class)

# For the complet model (training set)
df_complet_train <- data.frame(Class = raisin[train, "Class"], prob = prob_complet_train)
roc_complet_train <- roc(Class ~ prob, data = df_complet_train)

# For the complet model (test set)
df_complet_test <- data.frame(Class = raisin[test, "Class"], prob = prob_complet_test)
roc_complet_test <- roc(Class ~ prob, data = df_complet_test)

# For the PCA model (test set)
df_pca_test <- data.frame(Class = raisin[test, "Class"], prob = prob_pca_test)
roc_pca_test <- roc(Class ~ prob, data = df_pca_test)

# For the AIC model (test set)
df_aic_test <- data.frame(Class = raisin[test, "Class"], prob = prob_aic_test)
roc_aic_test <- roc(Class ~ prob, data = df_aic_test)

# For the Lasso model (test set)
df_lasso_test <- data.frame(Class = raisin[test, "Class"], prob = prob_lasso_test)
roc_lasso_test <- roc(Class ~ prob, data = df_lasso_test)

# For the SVM linear model (test set)
df_svm_linear_test <- data.frame(Class = raisin[test, "Class"], prob = prob_svm_linear_test)
roc_svm_linear_test <- roc(Class ~ prob, data = df_svm_linear_test)

# For the SVM polynomial model (test set)
df_svm_poly_test <- data.frame(Class = raisin[test, "Class"], prob = prob_svm_poly_test)
roc_svm_poly_test <- roc(Class ~ prob, data = df_svm_poly_test)

# Tracer les courbes ROC
plot(roc_complet_train, col = "blue", main = "Courbes ROC")
lines(roc_complet_test, col = "red")
lines(roc_pca_test, col = "green")
lines(roc_aic_test, col = "purple")
lines(roc_lasso_test, col = "orange")
lines(roc_svm_linear_test, col = "brown")
lines(roc_svm_poly_test, col = "pink")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Règle aléatoire

# Ajouter la légende avec les AUC
legend("bottomright", legend = c(
  paste("Complet (train), AUC =", round(auc(roc_complet_train), 2)),
  paste("Complet (test), AUC =", round(auc(roc_complet_test), 2)),
  paste("PCA, AUC =", round(auc(roc_pca_test), 2)),
  paste("AIC, AUC =", round(auc(roc_aic_test), 2)),
  paste("Lasso, AUC =", round(auc(roc_lasso_test), 2)),
  paste("SVM linéaire, AUC =", round(auc(roc_svm_linear_test), 2)),
  paste("SVM polynomial, AUC =", round(auc(roc_svm_poly_test), 2))
), col = c("blue", "red", "green", "purple", "orange", "brown", "pink"), lty = 1)

### QUESTION 6
# Fonction pour calculer l'erreur de classification ET la précision (pour la classe "Kecimen")
get_metrics = function(model, data, true_class) {
  if (inherits(model, "glm")) {
    # Logistic regression
    pred_prob = predict(model, newdata = data, type = "response")
    pred = ifelse(pred_prob > 0.5, "Kecimen", "Besni")
  } else if (inherits(model, "glmnet")) {
    # Lasso (glmnet)
    feature_data = as.matrix(data[, 1:7])
    pred_prob = predict(model, newx = feature_data, type = "response")[, 1]
    pred = ifelse(pred_prob > 0.5, "Kecimen", "Besni")
  } else if (inherits(model, "svm")) {
    # SVM
    pred = predict(model, newdata = data)
    pred = as.character(pred)
  } else {
    stop("Unsupported model type")
  }
  
  true_class_chr = as.character(true_class)
  error = mean(pred != true_class_chr)
  
  # Calcul précision pour la classe "Kecimen"
  true_positive = sum(pred == "Kecimen" & true_class_chr == "Kecimen")
  predicted_positive = sum(pred == "Kecimen")
  precision = ifelse(predicted_positive > 0, true_positive / predicted_positive, NA)
  
  return(list(error = error, precision = precision))
}

# Calcul des erreurs et précisions sur l'échantillon d'apprentissage
metrics_complet_train <- get_metrics(model_complet, raisin[train, ], raisin$Class[train])
metrics_pca_train <- get_metrics(model_pca, data.frame(raisin_train_pca, Class = raisin$Class[train]), raisin$Class[train])
metrics_aic_train <- get_metrics(model_aic, raisin[train, ], raisin$Class[train])
metrics_lasso_train <- get_metrics(model_lasso, raisin[train, ], raisin$Class[train])
metrics_svm_linear_train <- get_metrics(model_svm_linear, raisin[train, ], raisin$Class[train])
metrics_svm_poly_train <- get_metrics(model_svm_poly, raisin[train, ], raisin$Class[train])

# Calcul des erreurs et précisions sur l'échantillon de test
metrics_complet_test <- get_metrics(model_complet, raisin[test, ], raisin$Class[test])
metrics_pca_test <- get_metrics(model_pca, data.frame(raisin_test_proj$coord[,1:2], Class = raisin$Class[test]), raisin$Class[test])
metrics_aic_test <- get_metrics(model_aic, raisin[test, ], raisin$Class[test])
metrics_lasso_test <- get_metrics(model_lasso, raisin[test, ], raisin$Class[test])
metrics_svm_linear_test <- get_metrics(model_svm_linear, raisin[test, ], raisin$Class[test])
metrics_svm_poly_test <- get_metrics(model_svm_poly, raisin[test, ], raisin$Class[test])

# Affichage des résultats
cat("Échantillon d'apprentissage :\n")
cat(sprintf("Complet : erreur = %.4f, précision = %.4f\n", metrics_complet_train$error, metrics_complet_train$precision))
cat(sprintf("PCA : erreur = %.4f, précision = %.4f\n", metrics_pca_train$error, metrics_pca_train$precision))
cat(sprintf("AIC : erreur = %.4f, précision = %.4f\n", metrics_aic_train$error, metrics_aic_train$precision))
cat(sprintf("Lasso : erreur = %.4f, précision = %.4f\n", metrics_lasso_train$error, metrics_lasso_train$precision))
cat(sprintf("SVM linéaire : erreur = %.4f, précision = %.4f\n", metrics_svm_linear_train$error, metrics_svm_linear_train$precision))
cat(sprintf("SVM polynomial : erreur = %.4f, précision = %.4f\n", metrics_svm_poly_train$error, metrics_svm_poly_train$precision))

cat("\nÉchantillon de test :\n")
cat(sprintf("Complet : erreur = %.4f, précision = %.4f\n", metrics_complet_test$error, metrics_complet_test$precision))
cat(sprintf("PCA : erreur = %.4f, précision = %.4f\n", metrics_pca_test$error, metrics_pca_test$precision))
cat(sprintf("AIC : erreur = %.4f, précision = %.4f\n", metrics_aic_test$error, metrics_aic_test$precision))
cat(sprintf("Lasso : erreur = %.4f, précision = %.4f\n", metrics_lasso_test$error, metrics_lasso_test$precision))
cat(sprintf("SVM linéaire : erreur = %.4f, précision = %.4f\n", metrics_svm_linear_test$error, metrics_svm_linear_test$precision))
cat(sprintf("SVM polynomial : erreur = %.4f, précision = %.4f\n", metrics_svm_poly_test$error, metrics_svm_poly_test$precision))

# ... Parite 3-2
library(pROC)

# Projection du jeu de test (déjà fait)
Z_test <- raisin_test_proj$coord[,1:2]
y_test <- raisin$Class[test]

# Calcul du score LDA (score linéaire) à partir des coefficients a_hat et b_hat (partie 3-1-a)
scores_lda <- as.numeric(Z_test %*% a_hat - b_hat)

# Labels binaires (Besni = 1, Kecimen = 0)
labels_bin_test <- ifelse(y_test == "Besni", 1, 0)

# Calcul de la courbe ROC LDA
roc_lda <- roc(labels_bin_test, scores_lda, direction = "<")

# Tracer toutes les courbes ROC ensemble, y compris LDA
png(filename = "plots/partie_3/ROC_superposee_tous_modeles.png", width = 800, height = 600)

plot(roc_complet_test, col = "red", lwd = 2, main = "Courbes ROC des modèles sur jeu de test")
lines(roc_pca_test, col = "green", lwd = 2)
lines(roc_aic_test, col = "purple", lwd = 2)
lines(roc_lasso_test, col = "orange", lwd = 2)
lines(roc_svm_linear_test, col = "brown", lwd = 2)
lines(roc_svm_poly_test, col = "pink", lwd = 2)
lines(roc_lda, col = "darkcyan", lwd = 3, lty = 2)  # LDA en cyan et en tireté pour bien distinguer

abline(a=0,b=1,lty=2,col="gray")  # Diagonale aléatoire

legend("bottomright",
       legend = c(
         paste("Modèle complet (test), AUC =", round(auc(roc_complet_test), 2)),
         paste("PCA (test), AUC =", round(auc(roc_pca_test), 2)),
         paste("AIC (test), AUC =", round(auc(roc_aic_test), 2)),
         paste("Lasso (test), AUC =", round(auc(roc_lasso_test), 2)),
         paste("SVM linéaire (test), AUC =", round(auc(roc_svm_linear_test), 2)),
         paste("SVM polynomial (test), AUC =", round(auc(roc_svm_poly_test), 2)),
         paste("LDA (test), AUC =", round(auc(roc_lda), 2))
       ),
       col = c("red", "green", "purple", "orange", "brown", "pink", "darkcyan"),
       lwd = c(2, 2, 2, 2, 2, 2, 3),
       lty = c(1, 1, 1, 1, 1, 1, 2),
       bty = "n")

dev.off()

R

Copy
# Load required libraries
library(openxlsx)
library(MASS)
library(pROC)

# Create directory for plots (if not already created)
dir.create("plots/partie_3", showWarnings = FALSE)

# Load the Raisin dataset
raisin <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)

# Set seed for reproducibility
set.seed(1)

# Split data into training and test sets (2/3 training, 1/3 test)
n <- nrow(raisin)
train <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(2/3, 1/3))
test <- !train
X_train <- raisin[train, 1:7]  # All original variables
y_train <- raisin$Class[train]
X_test <- raisin[test, 1:7]
y_test <- raisin$Class[test]

# Standardize the explanatory variables
X_train_scaled <- scale(X_train, center = TRUE, scale = TRUE)
X_test_scaled <- scale(X_test, center = attr(X_train_scaled, "scaled:center"), 
                       scale = attr(X_train_scaled, "scaled:scale"))

# Fit LDA model on all original variables
lda_full <- lda(X_train_scaled, grouping = y_train)

# Predict on test set
pred_lda_full <- predict(lda_full, newdata = X_test_scaled)

# Compute LDA scores (posterior probabilities for Besni class)
scores_lda_full <- pred_lda_full$posterior[, "Besni"]

# Labels binaires (Besni = 1, Kecimen = 0)
labels_bin_test <- ifelse(y_test == "Besni", 1, 0)

# Calculate ROC curve for the new LDA model
roc_lda_full <- roc(labels_bin_test, scores_lda_full, direction = "<")

# Plot all ROC curves together, including PCA-based LDA and new LDA (full variables)
png(filename = "plots/partie_3/ROC_superposee_tous_modeles_with_lda_full.png", width = 800, height = 600)

plot(roc_complet_test, col = "red", lwd = 2, main = "Courbes ROC des modèles sur jeu de test")
lines(roc_pca_test, col = "green", lwd = 2)
lines(roc_aic_test, col = "purple", lwd = 2)
lines(roc_lasso_test, col = "orange", lwd = 2)
lines(roc_svm_linear_test, col = "brown", lwd = 2)
lines(roc_svm_poly_test, col = "pink", lwd = 2)
lines(roc_lda, col = "darkcyan", lwd = 3, lty = 2)  # PCA-based LDA
lines(roc_lda_full, col = "blue", lwd = 3, lty = 3)  # New LDA (full variables)

# Add random rule diagonal
abline(a = 0, b = 1, lty = 2, col = "gray")

# Update legend with AUC for all models
legend("bottomright",
       legend = c(
         paste("Modèle complet (test), AUC =", round(auc(roc_complet_test), 2)),
         paste("PCA (test), AUC =", round(auc(roc_pca_test), 2)),
         paste("AIC (test), AUC =", round(auc(roc_aic_test), 2)),
         paste("Lasso (test), AUC =", round(auc(roc_lasso_test), 2)),
         paste("SVM linéaire (test), AUC =", round(auc(roc_svm_linear_test), 2)),
         paste("SVM polynomial (test), AUC =", round(auc(roc_svm_poly_test), 2)),
         paste("LDA (PCA, test), AUC =", round(auc(roc_lda), 2)),
         paste("LDA (full, test), AUC =", round(auc(roc_lda_full), 2))
       ),
       col = c("red", "green", "purple", "orange", "brown", "pink", "darkcyan", "blue"),
       lwd = c(2, 2, 2, 2, 2, 2, 3, 3),
       lty = c(1, 1, 1, 1, 1, 1, 2, 3),
       bty = "n")

dev.off()

# Print AUC for reference
cat("AUC for LDA (full variables, test):", auc(roc_lda_full), "\n")