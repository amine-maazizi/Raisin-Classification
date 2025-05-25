# Effacer l'espace de travail et fermer les graphiques
rm(list = ls())
graphics.off()

# Charger toutes les bibliothèques nécessaires une seule fois
library(openxlsx)        # Pour lire le fichier Excel
library(ggplot2)         # Pour les visualisations
library(reshape2)        # Pour reformater les données
library(corrplot)        # Pour les matrices de corrélation
library(car)             # Pour les analyses statistiques
library(GGally)          # Pour les visualisations bivariées
library(FactoMineR)      # Pour l'ACP
library(cowplot)         # Pour organiser les graphiques
library(factoextra)      # Pour visualiser les résultats d'ACP et CAH
library(cluster)         # Pour la CAH
library(mclust)          # Pour l'indice ARI
library(pROC)            # Pour les courbes ROC
library(e1071)           # Pour les SVM
library(glmnet)          # Pour la régression Lasso
library(MASS)            # Pour LDA
library(biotools)        # Pour le test de Box
library(caret)           # Pour les modèles d'apprentissage
library(rpart)           # Pour les arbres de décision
library(randomForest)    # Pour les forêts aléatoires
library(xgboost)         # Pour le gradient boosting
library(class)           # Pour k-NN
library(nnet)            # Pour les réseaux de neurones
library(kableExtra)      # Pour les tableaux formatés
library(tictoc)          # Pour mesurer le temps d'exécution

# Créer les répertoires pour les sorties graphiques
dir.create("plots/partie_1", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/partie_1/analyse_uni_et_bi_variee", showWarnings = FALSE)
dir.create("plots/partie_1/ACP", showWarnings = FALSE)
dir.create("plots/partie_1/CAH", showWarnings = FALSE)
dir.create("plots/partie_1/CAH_sur_k_premieres_composantes", showWarnings = FALSE)
dir.create("plots/partie_2", showWarnings= FALSE)
dir.create("plots/partie_3", showWarnings = FALSE)

# Charger les données une seule fois
data <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)
data$Class <- as.factor(data$Class)  # Convertir la classe en facteur
n <- nrow(data)
p <- ncol(data)

# Vérifier les valeurs manquantes
cat("Vérification des valeurs manquantes :\n")
print(colSums(is.na(data)))

# Définir la graine pour reproductibilité
set.seed(1)

# Séparation en ensembles d'entraînement et de test (2/3 train, 1/3 test)
train <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(2/3, 1/3))
test <- !train
X_train <- data[train, 1:(p-1)]
y_train <- data$Class[train]
X_test <- data[test, 1:(p-1)]
y_test <- data$Class[test]

# Standardiser les données
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test, center = attr(X_train_scaled, "scaled:center"), 
                       scale = attr(X_train_scaled, "scaled:scale"))

# === Partie 1 : Analyse Exploratoire des Données ===

# Statistiques descriptives des variables morphologiques
morph_vars <- c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", 
                "Eccentricity", "ConvexArea", "Extent")
stats <- data.frame(
  Variable = morph_vars,
  Minimum = sapply(morph_vars, function(var) min(data[[var]], na.rm = TRUE)),
  Maximum = sapply(morph_vars, function(var) max(data[[var]], na.rm = TRUE)),
  Moyenne = sapply(morph_vars, function(var) mean(data[[var]], na.rm = TRUE))
)
cat("Statistiques descriptives des variables morphologiques :\n")
stats[, c("Minimum", "Maximum", "Moyenne")] <- round(stats[, c("Minimum", "Maximum", "Moyenne")], 3)
print(stats)
cat("\n")

# Boxplots facettés par classe
data_mod <- melt(data, id.vars="Class", measure.vars=1:(p-1))
p0 <- ggplot(data_mod, aes(x = as.factor(Class), y = value, fill = as.factor(Class))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  xlab("Classe") + ylab("Valeur") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(fill = "Classe") +
  theme(legend.position = "top")
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_all_faceted_by_class.png", plot = p0)

# Matrice de corrélation
cat("Matrice de corrélation :\n")
print(round(cor(data[,-p]), 2))

# Corrplot
png("plots/partie_1/analyse_uni_et_bi_variee/corrplot.png")
corrplot(cor(data[,-p]), method="circle")
dev.off()

# GGpairs avec classe
p1 <- ggpairs(data, ggplot2::aes(colour=as.factor(Class)))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/ggpairs_class.png", plot = p1)

# Analyse en Composantes Principales (ACP)
res_pca <- PCA(X_train, scale.unit = TRUE, graph = FALSE, ncp=7)

# Éboulis des valeurs propres
cat("Valeurs propres de l'ACP :\n")
print(round(res_pca$eig, 4))
cat("Somme des valeurs propres :", sum(res_pca$eig[,1]), "\n")

p_eig <- fviz_eig(res_pca, addlabels = TRUE, main = "Éboulis des valeurs propres") +
  geom_hline(yintercept = 100/p)
ggsave("plots/partie_1/ACP/scree_plot.jpg", plot = p_eig)

# Cercles de corrélation
p_var_1_2 <- fviz_pca_var(res_pca, axes = c(1, 2), repel = TRUE)
p_var_2_3 <- fviz_pca_var(res_pca, axes = c(2, 3), repel = TRUE)
ggsave("plots/partie_1/ACP/cor_circle_1_2.jpg", plot = p_var_1_2)
ggsave("plots/partie_1/ACP/cor_circle_2_3.jpg", plot = p_var_2_3)

# Plans factoriels des individus
p_ind_1_2 <- fviz_pca_ind(res_pca, habillage = as.factor(y_train), addEllipses = TRUE, 
                          label = "none", repel = TRUE)
p_ind_2_3 <- fviz_pca_ind(res_pca, axes = c(2, 3), habillage = as.factor(y_train), 
                          addEllipses = TRUE, label = "none", repel = TRUE)
ggsave("plots/partie_1/ACP/ind_plan1_2.jpg", plot = p_ind_1_2, width = 12, height = 6)
ggsave("plots/partie_1/ACP/ind_plan_2_3.jpg", plot = p_ind_2_3, width = 12, height = 6)

# Classification Ascendante Hiérarchique (CAH)
# Données standardisées
dist_scaled <- dist(X_train_scaled, method = "euclidean")
cah_scaled <- hclust(dist_scaled, method = "ward.D2")
png("plots/partie_1/CAH/dendrogramme_normalise.png", width = 800, height = 600)
plot(cah_scaled, main = "CAH avec normalisation", labels = FALSE)
rect.hclust(cah_scaled, k = 2, border = "blue")
dev.off()

# Hauteur des fusions
png("plots/partie_1/CAH/fusion_heights.png", width = 800, height = 600)
plot(rev(cah_scaled$height), type = "b", pch = 19,
     xlab = "Fusion (ordre inverse)", ylab = "Hauteur",
     main = "Hauteur des fusions (Ward)")
dev.off()

# Nombre optimal de clusters
png("plots/partie_1/CAH/silhouette_nbclust.png", width = 800, height = 600)
fviz_nbclust(X_train_scaled, FUN = hcut, method = "silhouette") +
  ggtitle("Nombre optimal de clusters - Méthode silhouette (Ward)")
dev.off()

# Silhouette pour k=2
grp_scaled <- cutree(cah_scaled, k = 2)
sil_scaled <- silhouette(grp_scaled, dist_scaled)
p_sil_scaled <- fviz_silhouette(sil_scaled, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_scaled.jpg", plot = p_sil_scaled)
cat("Indice de silhouette moyen (k=2, normalisé) :", round(mean(sil_scaled[, 3]), 3), "\n")

# ARI et matrice de confusion
ari_scaled <- adjustedRandIndex(grp_scaled, y_train)
cat("ARI avec normalisation :", round(ari_scaled, 3), "\n")
conf_matrix_scaled <- table(y_train, grp_scaled)
error_rate_scaled <- 1 - sum(diag(conf_matrix_scaled)) / sum(conf_matrix_scaled)
cat("Erreur de classification (k=2, normalisé) :", round(error_rate_scaled, 3), "\n")

# Données brutes
dist_raw <- dist(X_train, method = "euclidean")
cah_raw <- hclust(dist_raw, method = "ward.D2")
png("plots/partie_1/CAH/dendrogramme_non_normalise.png", width = 800, height = 600)
plot(cah_raw, main = "CAH sans normalisation", labels = FALSE)
rect.hclust(cah_raw, k = 2, border = "red")
dev.off()

# Silhouette pour k=2 (brut)
grp_raw <- cutree(cah_raw, k = 2)
sil_raw <- silhouette(grp_raw, dist_raw)
p_sil_raw <- fviz_silhouette(sil_raw, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_raw.jpg", plot = p_sil_raw)
cat("Indice de silhouette moyen (k=2, brut) :", round(mean(sil_raw[, 3]), 3), "\n")

# ARI et matrice de confusion (brut)
ari_raw <- adjustedRandIndex(grp_raw, y_train)
cat("ARI sans normalisation :", round(ari_raw, 3), "\n")
conf_matrix_raw <- table(y_train, grp_raw)
error_rate_raw <- 1 - sum(diag(conf_matrix_raw)) / sum(conf_matrix_raw)
cat("Erreur de classification (k=2, brut) :", round(error_rate_raw, 3), "\n")

# CAH sur les k premières composantes principales
compute_cah_metrics <- function(k) {
  pc_k <- res_pca$ind$coord[, 1:k]
  dist_k <- dist(pc_k)
  cah_k <- hclust(dist_k, method = "ward.D2")
  grp_k <- cutree(cah_k, k = 2)
  conf_mat <- table(y_train, grp_k)
  error <- 1 - sum(diag(conf_mat)) / sum(conf_mat)
  ari <- adjustedRandIndex(grp_k, y_train)
  png(paste0("plots/partie_1/CAH_sur_k_premieres_composantes/dendro_k", k, ".png"))
  plot(cah_k, main = paste("Dendrogramme sur", k, "composantes principales"), labels = FALSE)
  rect.hclust(cah_k, k = 2, border = "darkgreen")
  dev.off()
  return(c(Taux_Erreur = error, ARI = ari))
}

results_list <- lapply(1:7, compute_cah_metrics)
error_df <- data.frame(Nb_CP = 1:7, do.call(rbind, results_list))
error_df$Taux_Erreur <- round(error_df$Taux_Erreur, 4)
error_df$ARI <- round(error_df$ARI, 4)
cat("Résultats de la CAH sur les composantes principales :\n")
print(error_df)

p_err <- ggplot(error_df, aes(x = Nb_CP)) +
  geom_line(aes(y = Taux_Erreur), color = "red") +
  geom_point(aes(y = Taux_Erreur), color = "red") +
  ylab("Taux d'erreur de classification") +
  xlab("Nombre de composantes principales") +
  ggtitle("Taux d'erreur en fonction du nombre de CP")
ggsave("plots/partie_1/CAH_sur_k_premieres_composantes/taux_erreur_par_k.png", plot = p_err)

best_k <- which.min(error_df$Taux_Erreur)
cat("Meilleur k pour CAH :", best_k, "avec un taux d'erreur de", error_df$Taux_Erreur[best_k], 
    "et ARI de", error_df$ARI[best_k], "\n")

# === Partie 2 : Modèles de Classification ===

# Projection des données de test pour ACP
test_pca <- predict(res_pca, newdata = X_test)$coord[, 1:2]

# Modèle complet (régression logistique)
model_complet <- glm(Class ~ ., data = data[train, ], family = binomial)

# Modèle avec deux premières composantes principales
train_pca_data <- data.frame(Dim.1 = res_pca$ind$coord[,1], Dim.2 = res_pca$ind$coord[,2], Class = y_train)
model_pca <- glm(Class ~ Dim.1 + Dim.2, data = train_pca_data, family = binomial)

# Modèle avec sélection par AIC
model_aic <- step(model_complet, direction = "both", trace = FALSE)

# Modèle Lasso
X_train_matrix <- as.matrix(X_train)
cv_lasso <- cv.glmnet(X_train_matrix, as.numeric(y_train) - 1, family = "binomial", alpha = 1)
model_lasso <- glmnet(X_train_matrix, as.numeric(y_train) - 1, family = "binomial", 
                      alpha = 1, lambda = cv_lasso$lambda.min)

# SVM linéaire
model_svm_linear <- svm(Class ~ ., data = data[train, ], kernel = "linear", probability = TRUE)

# SVM polynomial (degré 3)
model_svm_poly <- svm(Class ~ ., data = data[train, ], kernel = "polynomial", degree = 3, probability = TRUE)

# Fonction pour obtenir les probabilités prédites
get_prob <- function(model, data, pca_data = NULL) {
  if (inherits(model, "glm")) {
    return(predict(model, newdata = data, type = "response"))
  } else if (inherits(model, "svm")) {
    return(attr(predict(model, newdata = data, probability = TRUE), "probabilities")[,2])
  } else if (inherits(model, "glmnet")) {
    return(predict(model, newx = as.matrix(data[,1:7]), type = "response")[,1])
  }
}

# Calcul des probabilités pour les courbes ROC
prob_complet_train <- get_prob(model_complet, data[train, ])
prob_complet_test <- get_prob(model_complet, data[test, ])
prob_pca_test <- predict(model_pca, newdata = data.frame(Dim.1 = test_pca[,1], Dim.2 = test_pca[,2]), type = "response")
prob_aic_test <- get_prob(model_aic, data[test, ])
prob_lasso_test <- get_prob(model_lasso, X_test)
prob_svm_linear_test <- get_prob(model_svm_linear, data[test, ])
prob_svm_poly_test <- get_prob(model_svm_poly, data[test, ])

# Courbes ROC
roc_complet_train <- roc(y_train, prob_complet_train)
roc_complet_test <- roc(y_test, prob_complet_test)
roc_pca_test <- roc(y_test, prob_pca_test)
roc_aic_test <- roc(y_test, prob_aic_test)
roc_lasso_test <- roc(y_test, prob_lasso_test)
roc_svm_linear_test <- roc(y_test, prob_svm_linear_test)
roc_svm_poly_test <- roc(y_test, prob_svm_poly_test)

# Tracer les courbes ROC
png("plots/partie_2/ROC_all_models.png", width = 800, height = 600)
plot(roc_complet_train, col = "blue", main = "Courbes ROC des modèles")
lines(roc_complet_test, col = "red")
lines(roc_pca_test, col = "green")
lines(roc_aic_test, col = "purple")
lines(roc_lasso_test, col = "orange")
lines(roc_svm_linear_test, col = "brown")
lines(roc_svm_poly_test, col = "pink")
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = c(
  paste("Complet (train), AUC =", round(auc(roc_complet_train), 2)),
  paste("Complet (test), AUC =", round(auc(roc_complet_test), 2)),
  paste("PCA, AUC =", round(auc(roc_pca_test), 2)),
  paste("AIC, AUC =", round(auc(roc_aic_test), 2)),
  paste("Lasso, AUC =", round(auc(roc_lasso_test), 2)),
  paste("SVM linéaire, AUC =", round(auc(roc_svm_linear_test), 2)),
  paste("SVM polynomial, AUC =", round(auc(roc_svm_poly_test), 2))
), col = c("blue", "red", "green", "purple", "orange", "brown", "pink"), lty = 1)
dev.off()

# Calcul des erreurs et précisions
get_metrics <- function(model, data, true_class, pca_data = NULL) {
  if (inherits(model, "glm")) {
    pred_prob <- predict(model, newdata = data, type = "response")
    pred <- ifelse(pred_prob > 0.5, "Kecimen", "Besni")
  } else if (inherits(model, "glmnet")) {
    pred_prob <- predict(model, newx = as.matrix(data[,1:7]), type = "response")[,1]
    pred <- ifelse(pred_prob > 0.5, "Kecimen", "Besni")
  } else if (inherits(model, "svm")) {
    pred <- as.character(predict(model, newdata = data))
  } else {
    stop("Type de modèle non supporté")
  }
  error <- mean(pred != true_class)
  true_positive <- sum(pred == "Kecimen" & true_class == "Kecimen")
  predicted_positive <- sum(pred == "Kecimen")
  precision <- ifelse(predicted_positive > 0, true_positive / predicted_positive, NA)
  return(list(error = error, precision = precision))
}

# Calcul des métriques
metrics_complet_train <- get_metrics(model_complet, data[train, ], y_train)
metrics_pca_train <- get_metrics(model_pca, train_pca_data, y_train)
metrics_aic_train <- get_metrics(model_aic, data[train, ], y_train)
metrics_lasso_train <- get_metrics(model_lasso, data[train, ], y_train)
metrics_svm_linear_train <- get_metrics(model_svm_linear, data[train, ], y_train)
metrics_svm_poly_train <- get_metrics(model_svm_poly, data[train, ], y_train)

metrics_complet_test <- get_metrics(model_complet, data[test, ], y_test)
metrics_pca_test <- get_metrics(model_pca, data.frame(Dim.1 = test_pca[,1], Dim.2 = test_pca[,2]), y_test)
metrics_aic_test <- get_metrics(model_aic, data[test, ], y_test)
metrics_lasso_test <- get_metrics(model_lasso, data[test, ], y_test)
metrics_svm_linear_test <- get_metrics(model_svm_linear, data[test, ], y_test)
metrics_svm_poly_test <- get_metrics(model_svm_poly, data[test, ], y_test)

# Affichage des résultats
cat("Échantillon d'entraînement :\n")
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

# === Partie 3 : Analyse Discriminante Linéaire (LDA) ===

# LDA sur les deux premières composantes principales
Z_train <- res_pca$ind$coord[, 1:2]
Z_test <- test_pca
Z0 <- Z_train[y_train == "Kecimen", ]
Z1 <- Z_train[y_train == "Besni", ]
mu0_hat <- colMeans(Z0)
mu1_hat <- colMeans(Z1)
S0 <- cov(Z0)
S1 <- cov(Z1)
n0 <- nrow(Z0)
n1 <- nrow(Z1)
Sigma_hat <- ((n0 - 1) * S0 + (n1 - 1) * S1) / (n0 + n1 - 2)
a_hat <- solve(Sigma_hat) %*% (mu1_hat - mu0_hat)
b_hat <- as.numeric(0.5 * (t(mu1_hat) %*% solve(Sigma_hat) %*% mu1_hat - 
                             t(mu0_hat) %*% solve(Sigma_hat) %*% mu0_hat))
cat("Coefficients LDA (a_hat) :\n")
print(a_hat)
cat("Intercept LDA (b_hat) :", b_hat, "\n")

# Vérification avec décomposition spectrale
eig <- eigen(Sigma_hat)
U <- eig$vectors
D_inv <- diag(1/eig$values)
Sigma_inv_eig <- U %*% D_inv %*% t(U)
a_hat_eig <- Sigma_inv_eig %*% (mu1_hat - mu0_hat)
b_hat_eig <- as.numeric(0.5 * (t(mu1_hat) %*% Sigma_inv_eig %*% mu1_hat - 
                                 t(mu0_hat) %*% Sigma_inv_eig %*% mu0_hat))
cat("Vérification décomposition spectrale (a_hat_eig) :\n")
print(a_hat_eig)
cat("Intercept (b_hat_eig) :", b_hat_eig, "\n")
if (all.equal(b_hat, b_hat_eig, tolerance = 1e-10)) {
  cat("Vérification OK : b_hat et b_hat_eig sont égaux\n")
} else {
  cat("Différence détectée entre b_hat et b_hat_eig\n")
}

# Visualisation de la frontière de décision
png("plots/partie_3/lda_boundary.png", width = 800, height = 600)
plot(Z_train[, 1], Z_train[, 2], col = ifelse(y_train == "Kecimen", "blue", "red"), 
     xlab = "Dim.1", ylab = "Dim.2", pch = 16)
abline(a = b_hat / a_hat[2], b = -a_hat[1] / a_hat[2], col = "purple")
legend("topright", legend = c("Kecimen", "Besni"), col = c("blue", "red"), pch = 16)
dev.off()

# Prédictions LDA sur le jeu de test
pred_lda <- Z_test %*% a_hat > b_hat
y_pred_lda <- ifelse(pred_lda, "Besni", "Kecimen")
error_rate_lda <- mean(y_pred_lda != y_test)
cat("Taux d'erreur LDA (ACP) :", error_rate_lda, "\n")

# LDA avec MASS
lda_model <- lda(Z_train, grouping = y_train)
pred_lda_mass <- predict(lda_model, newdata = Z_test)$class
error_rate_lda_mass <- mean(pred_lda_mass != y_test)
cat("Taux d'erreur LDA (MASS, ACP) :", error_rate_lda_mass, "\n")

# LDA sur toutes les variables
lda_full <- lda(X_train_scaled, grouping = y_train)
pred_lda_full <- predict(lda_full, newdata = X_test_scaled)
y_pred_full <- pred_lda_full$class
error_rate_full <- mean(y_pred_full != y_test)
cat("Taux d'erreur LDA (toutes variables) :", error_rate_full, "\n")

# Courbe ROC pour LDA
scores_lda <- Z_test %*% a_hat - b_hat
labels_bin_test <- ifelse(y_test == "Besni", 1, 0)
roc_lda <- roc(labels_bin_test, scores_lda, direction = "<")
scores_lda_full <- pred_lda_full$posterior[, "Besni"]
roc_lda_full <- roc(labels_bin_test, scores_lda_full, direction = "<")

# Tracer toutes les courbes ROC
png("plots/partie_3/ROC_superposee_tous_modeles.png", width = 800, height = 600)
plot(roc_complet_test, col = "red", lwd = 2, main = "Courbes ROC des modèles sur jeu de test")
lines(roc_pca_test, col = "green", lwd = 2)
lines(roc_aic_test, col = "purple", lwd = 2)
lines(roc_lasso_test, col = "orange", lwd = 2)
lines(roc_svm_linear_test, col = "brown", lwd = 2)
lines(roc_svm_poly_test, col = "pink", lwd = 2)
lines(roc_lda, col = "darkcyan", lwd = 3, lty = 2)
lines(roc_lda_full, col = "blue", lwd = 3, lty = 3)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = c(
  paste("Complet (test), AUC =", round(auc(roc_complet_test), 2)),
  paste("PCA (test), AUC =", round(auc(roc_pca_test), 2)),
  paste("AIC (test), AUC =", round(auc(roc_aic_test), 2)),
  paste("Lasso (test), AUC =", round(auc(roc_lasso_test), 2)),
  paste("SVM linéaire (test), AUC =", round(auc(roc_svm_linear_test), 2)),
  paste("SVM polynomial (test), AUC =", round(auc(roc_svm_poly_test), 2)),
  paste("LDA (PCA, test), AUC =", round(auc(roc_lda), 2)),
  paste("LDA (full, test), AUC =", round(auc(roc_lda_full), 2))
), col = c("red", "green", "purple", "orange", "brown", "pink", "darkcyan", "blue"),
lwd = c(2, 2, 2, 2, 2, 2, 3, 3), lty = c(1, 1, 1, 1, 1, 1, 2, 3), bty = "n")
dev.off()

# Test de Box pour QDA vs LDA
numeric_cols <- sapply(data, is.numeric)
vars_numeric <- names(data)[numeric_cols]
cov_list <- lapply(levels(data$Class), function(cl) cov(data[data$Class == cl, vars_numeric]))
names(cov_list) <- levels(data$Class)
cat("Matrice de covariance par classe :\n")
print(cov_list)
box_test <- boxM(data[, vars_numeric], data$Class)
cat("Test de Box pour l'égalité des covariances :\n")
print(box_test)
cat("p-valeur du test de Box :", box_test$p.value, "\n")

# === BONUS : Comparaison de Modèles Additionnels ===

# Fonction d'évaluation
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

# Régression logistique
tic()
logreg_model <- glm(Class ~ ., data = data[train, ], family = binomial)
t_train <- toc(log = TRUE)
tic()
logreg_pred_train <- predict(logreg_model, data[train, ], type = "response")
logreg_pred_test <- predict(logreg_model, data[test, ], type = "response")
logreg_pred_train <- factor(ifelse(logreg_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
logreg_pred_test <- factor(ifelse(logreg_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression"]] <- evaluate_model(logreg_pred_train, logreg_pred_test,
                                                   t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Régression logistique avec PCA
tic()
train_pca_data <- data.frame(Dim.1 = res_pca$ind$coord[,1], Dim.2 = res_pca$ind$coord[,2], Class = y_train)
pca_logreg_model <- glm(Class ~ Dim.1 + Dim.2, data = train_pca_data, family = binomial)
t_train <- toc(log = TRUE)
tic()
test_pca_data <- data.frame(Dim.1 = test_pca[,1], Dim.2 = test_pca[,2])
pca_pred_train <- predict(pca_logreg_model, train_pca_data, type = "response")
pca_pred_test <- predict(pca_logreg_model, test_pca_data, type = "response")
pca_pred_train <- factor(ifelse(pca_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
pca_pred_test <- factor(ifelse(pca_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression PCA"]] <- evaluate_model(pca_pred_train, pca_pred_test,
                                                       t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Régression logistique avec AIC
tic()
logreg_aic_model <- step(logreg_model, direction = "both", trace = FALSE)
t_train <- toc(log = TRUE)
tic()
aic_pred_train <- predict(logreg_aic_model, data[train, ], type = "response")
aic_pred_test <- predict(logreg_aic_model, data[test, ], type = "response")
aic_pred_train <- factor(ifelse(aic_pred_train > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
aic_pred_test <- factor(ifelse(aic_pred_test > 0.5, levels(y_train)[2], levels(y_train)[1]), levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Logistic Regression AIC"]] <- evaluate_model(aic_pred_train, aic_pred_test,
                                                       t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Régression Lasso
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
                                                         t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# SVM linéaire
tic()
svm_linear_model <- svm(Class ~ ., data = data[train, ], kernel = "linear", probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_linear_pred_train <- predict(svm_linear_model, X_train)
svm_linear_pred_test <- predict(svm_linear_model, X_test)
t_inf <- toc(log = TRUE)
results[["Linear SVM"]] <- evaluate_model(svm_linear_pred_train, svm_linear_pred_test,
                                          t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# SVM polynomial
tic()
svm_poly_model <- svm(Class ~ ., data = data[train, ], kernel = "polynomial", degree = 3, probability = TRUE)
t_train <- toc(log = TRUE)
tic()
svm_poly_pred_train <- predict(svm_poly_model, X_train)
svm_poly_pred_test <- predict(svm_poly_model, X_test)
t_inf <- toc(log = TRUE)
results[["Polynomial SVM"]] <- evaluate_model(svm_poly_pred_train, svm_poly_pred_test,
                                              t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Arbre de décision
tic()
dt_model <- rpart(Class ~ ., data = data[train, ], method = "class")
t_train <- toc(log = TRUE)
tic()
dt_pred_train <- predict(dt_model, X_train, type = "class")
dt_pred_test <- predict(dt_model, X_test, type = "class")
t_inf <- toc(log = TRUE)
results[["Decision Tree"]] <- evaluate_model(dt_pred_train, dt_pred_test,
                                             t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Forêt aléatoire
tic()
rf_model <- randomForest(Class ~ ., data = data[train, ], ntree = 100)
t_train <- toc(log = TRUE)
tic()
rf_pred_train <- predict(rf_model, X_train)
rf_pred_test <- predict(rf_model, X_test)
t_inf <- toc(log = TRUE)
results[["Random Forest"]] <- evaluate_model(rf_pred_train, rf_pred_test,
                                             t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Gradient Boosting (XGBoost)
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
                                                 t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Naive Bayes
tic()
nb_model <- naiveBayes(Class ~ ., data = data[train, ])
t_train <- toc(log = TRUE)
tic()
nb_pred_train <- predict(nb_model, X_train)
nb_pred_test <- predict(nb_model, X_test)
t_inf <- toc(log = TRUE)
results[["Naive Bayes"]] <- evaluate_model(nb_pred_train, nb_pred_test,
                                           t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# k-NN
tic()
knn_pred_train <- knn(train = X_train, test = X_train, cl = y_train, k = 5)
t_train <- toc(log = TRUE)
tic()
knn_pred_test <- knn(train = X_train, test = X_test, cl = y_train, k = 5)
t_inf <- toc(log = TRUE)
results[["k-NN"]] <- evaluate_model(knn_pred_train, knn_pred_test,
                                    t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Réseau de neurones
tic()
nn_model <- nnet(Class ~ ., data = data[train, ], size = 4, decay = 0.0001, maxit = 500, trace = FALSE)
t_train <- toc(log = TRUE)
tic()
nn_pred_train <- predict(nn_model, X_train, type = "class")
nn_pred_test <- predict(nn_model, X_test, type = "class")
nn_pred_train <- factor(nn_pred_train, levels = levels(y_train))
nn_pred_test <- factor(nn_pred_test, levels = levels(y_train))
t_inf <- toc(log = TRUE)
results[["Neural Network"]] <- evaluate_model(nn_pred_train, nn_pred_test,
                                              t_train$toc - t_train$tic, t_inf$toc - t_inf$tic, y_train, y_test)

# Combiner les résultats
results_df <- do.call(rbind, results)
results_df <- cbind(Modèle = rownames(results_df), results_df)
rownames(results_df) <- NULL

# Afficher le tableau des résultats
cat("Comparaison des modèles (BONUS) :\n")
kable(results_df, format = "html", digits = 3, booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, position = "center")
```