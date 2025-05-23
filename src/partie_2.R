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










