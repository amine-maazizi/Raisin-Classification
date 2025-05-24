library(readxl)
library(FactoMineR)

# Charger les données
raisin = read_excel("../dataset/Raisin.xlsx")

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




##### Question 5

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
# Fonction pour calculer l'erreur de classification
get_error <- function(model, data, true_class) {
  if (inherits(model, "glm")) {
    pred <- predict(model, newdata = data, type = "response") > 0.5
  } else if (inherits(model, "svm")) {
    pred <- predict(model, newdata = data)
  } else if (inherits(model, "glmnet")) {
    pred <- predict(model, newx = as.matrix(data[,1:7]), type = "response") > 0.5
  }
  return(mean(pred != true_class))
}

# Erreurs sur l'échantillon d'apprentissage
error_complet_train <- get_error(model_complet, raisin[train, ], raisin[train, "Class"])
error_pca_train <- get_error(model_pca, data.frame(raisin_train_pca, Class = raisin[train, "Class"]), raisin[train, "Class"])
error_aic_train <- get_error(model_aic, raisin[train, ], raisin[train, "Class"])
error_lasso_train <- get_error(model_lasso, raisin[train, ], raisin[train, "Class"])
error_svm_linear_train <- get_error(model_svm_linear, raisin[train, ], raisin$Class[train])
error_svm_poly_train <- get_error(model_svm_poly, raisin[train, ], raisin$Class[train])

# Erreurs sur l'échantillon de test
error_complet_test <- get_error(model_complet, raisin[test, ], raisin[test, "Class"])
error_pca_test <- get_error(model_pca, data.frame(raisin_test_proj$coord[,1:2], Class = raisin[test, "Class"]), raisin[test, "Class"])
error_aic_test <- get_error(model_aic, raisin[test, ], raisin[test, "Class"])
error_lasso_test <- get_error(model_lasso, raisin[test, ], raisin[test, "Class"])
error_svm_linear_test <- get_error(model_svm_linear, raisin[test, ], raisin$Class[test])
error_svm_poly_test <- get_error(model_svm_poly, raisin[test, ], raisin$Class[test])

# Afficher les erreurs
cat("Erreur sur l'échantillon d'apprentissage:\n")
cat("Complet:", error_complet_train, "\n")
cat("PCA:", error_pca_train, "\n")
cat("AIC:", error_aic_train, "\n")
cat("Lasso:", error_lasso_train, "\n")
cat("SVM linéaire:", error_svm_linear_train, "\n")
cat("SVM polynomial:", error_svm_poly_train, "\n")

cat("\nErreur sur l'échantillon de test:\n")
cat("Complet:", error_complet_test, "\n")
cat("PCA:", error_pca_test, "\n")
cat("AIC:", error_aic_test, "\n")
cat("Lasso:", error_lasso_test, "\n")
cat("SVM linéaire:", error_svm_linear_test, "\n")
cat("SVM polynomial:", error_svm_poly_test, "\n")




