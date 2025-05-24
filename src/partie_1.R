# Clear workspace and close graphics
rm(list = objects())
graphics.off()

# Create necessary directories if they don't exist
dir.create("plots/partie_1", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/partie_1/analyse_uni_et_bi_variee", showWarnings = FALSE)
dir.create("plots/partie_1/ACP", showWarnings = FALSE)
dir.create("plots/partie_1/CAH", showWarnings = FALSE)
dir.create("plots/partie_1/CAH_sur_k_premieres_composantes", showWarnings = FALSE)

# Chargement des données
library(openxlsx)
data <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)

n = nrow(data)
p = ncol(data)

colSums(is.na(data))

# Vérification des valeurs revendiquées par l'article
morph_vars <- c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", 
                "Eccentricity", "ConvexArea", "Extent")

# Calcul des statistiques descriptives avec sapply
stats <- data.frame(
  Variable = morph_vars,
  Minimum = sapply(morph_vars, function(var) min(data[[var]], na.rm = TRUE)),
  Maximum = sapply(morph_vars, function(var) max(data[[var]], na.rm = TRUE)),
  Moyenne = sapply(morph_vars, function(var) mean(data[[var]], na.rm = TRUE))
)

# Affichage des résultats avec formatage
cat("Statistiques descriptives des variables morphologiques :\n")
stats[, c("Minimum", "Maximum", "Moyenne")] <- round(stats[, c("Minimum", "Maximum", "Moyenne")], 3)
print(stats)  
cat("\n")

library(ggplot2)
library(reshape2)
library(corrplot)
library(car)
library(GGally)

# Analyse univariée et bivariée
# Boxplots facettés
data_mod <- melt(data, id.vars="Class", measure.vars=1:(p-1))
p0 <- ggplot(data_mod, aes(x = as.factor(Class), y = value, fill = as.factor(Class))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +  # Échelle libre pour l'axe y
  xlab("Classe") + ylab("Valeur") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(fill = "Classe") +  # Légende pour les couleurs
  theme(legend.position = "top")  # Position de la légende
p0
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_all_faceted_by_class.png", plot = p0)

# Corrélation
round(cor(data[,-p]), 2)

# Corrplot
png("plots/partie_1/analyse_uni_et_bi_variee/corrplot.png")
corrplot(cor(data[,-p]), method="circle")
dev.off()

# GGpairs avec Class
p1 <- ggpairs(data, ggplot2::aes(colour=as.factor(Class)))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/ggpairs_class.png", plot = p1)


# ACP
library(FactoMineR)
library(cowplot)
library(factoextra)

class_vector <- data$Class      
res <- PCA(data[, -p], scale.unit = TRUE, graph = FALSE, ncp=7)

# a - Valeurs et vecteurs propres
round(res$eig, 4)
sum(res$eig[,1])

p1 <- fviz_eig(res, addlabels = TRUE, main = "Éboulis des valeurs propres") +
  geom_hline(yintercept = 100/p)
ggsave("plots/partie_1/ACP/scree_plot.jpg", plot = p1)


# Create ggplot-based correlation circle plots
p2 <- fviz_pca_var(res, axes = c(1, 2), repel = TRUE)
p3 <- fviz_pca_var(res, axes = c(2, 3), repel = TRUE)

# Save them using ggsave
ggsave("plots/partie_1/ACP/cor_circle_1_2.jpg", plot = p2)
ggsave("plots/partie_1/ACP/cor_circle_2_3.jpg", plot = p3)

# Plan 1-2 : individus colorés par classe et variables
p_ind_1_2 <- fviz_pca_ind(res, 
                          habillage = as.factor(class_vector), 
                          addEllipses = TRUE, 
                          label = "none", 
                          repel = TRUE)


ggsave("plots/partie_1/ACP/ind_plan1_2.jpg", plot = p_ind_1_2, width = 12, height = 6)


# Plan 2-3 : même chose avec axes 2 et 3
p_ind_2_3 <- fviz_pca_ind(res, 
                          habillage = as.factor(class_vector), 
                          axes = c(2, 3), 
                          addEllipses = TRUE, 
                          label = "none", 
                          repel = TRUE)


ggsave("plots/partie_1/ACP/ind_plan_2_3.jpg", plot = p_ind_2_3, width = 12, height = 6)


library(cluster)
library(mclust)
library(factoextra)  # pour fviz_silhouette et fviz_nbclust

# ========================================
# Données standardisées
# ========================================
data_scaled <- scale(data[, -p])
dist_scaled <- dist(data_scaled, method = "euclidean")
cah_scaled <- hclust(dist_scaled, method = "ward.D2")

# Dendrogramme normalisé
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

# Choix du nombre optimal de clusters
png("plots/partie_1/CAH/silhouette_nbclust.png", width = 800, height = 600)
fviz_nbclust(data_scaled, FUN = hcut, method = "silhouette") +
  ggtitle("Nombre optimal de clusters - Méthode silhouette (Ward)")
dev.off()

# Silhouette pour k = 2
grp_scaled <- cutree(cah_scaled, k = 2)
sil_scaled <- silhouette(grp_scaled, dist_scaled)

p_sil_scaled <- fviz_silhouette(sil_scaled, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_scaled.jpg", plot = p_sil_scaled)

sil_mean_scaled <- mean(sil_scaled[, 3])
cat("Indice de silhouette moyen (k=2, normalisé) :", round(sil_mean_scaled, 3), "\n")

# ARI et matrice de confusion
ari_scaled <- adjustedRandIndex(grp_scaled, data$Class)
cat("ARI avec normalisation :", round(ari_scaled, 3), "\n")

conf_matrix_scaled <- table(data$Class, grp_scaled)
error_rate_scaled <- 1 - sum(diag(conf_matrix_scaled)) / sum(conf_matrix_scaled)
cat("Erreur de classification (k=2, normalisé) :", round(error_rate_scaled, 3), "\n")

# ========================================
# Données brutes (non standardisées)
# ========================================
data_raw <- data[, -p]
dist_raw <- dist(data_raw, method = "euclidean")
cah_raw <- hclust(dist_raw, method = "ward.D2")

# Dendrogramme brut
png("plots/partie_1/CAH/dendrogramme_non_normalise.png", width = 800, height = 600)
plot(cah_raw, main = "CAH sans normalisation", labels = FALSE)
rect.hclust(cah_raw, k = 2, border = "red")
dev.off()

# Silhouette pour k = 2
grp_raw <- cutree(cah_raw, k = 2)
sil_raw <- silhouette(grp_raw, dist_raw)

p_sil_raw <- fviz_silhouette(sil_raw, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_raw.jpg", plot = p_sil_raw)

sil_mean_raw <- mean(sil_raw[, 3])
cat("Indice de silhouette moyen (k=2, brut) :", round(sil_mean_raw, 3), "\n")

# ARI et matrice de confusion
ari_raw <- adjustedRandIndex(grp_raw, data$Class)
cat("ARI sans normalisation :", round(ari_raw, 3), "\n")

conf_matrix_raw <- table(data$Class, grp_raw)
error_rate_raw <- 1 - sum(diag(conf_matrix_raw)) / sum(conf_matrix_raw)
cat("Erreur de classification (k=2, brut) :", round(error_rate_raw, 3), "\n")

# ===============================
# Q4 : CAH sur les k premières CP 
# ===============================


# Fonction pour calculer l'erreur et l'ARI pour un nombre donné de CP
compute_cah_metrics <- function(k) {
  pc_k <- res$ind$coord[, 1:k]
  dist_k <- dist(pc_k)
  cah_k <- hclust(dist_k, method = "ward.D2")
  grp_k <- cutree(cah_k, k = 2)
  
  # Matrice de confusion
  conf_mat <- table(data$Class, grp_k)
  error <- 1 - sum(diag(conf_mat)) / sum(conf_mat)
  ari <- adjustedRandIndex(grp_k, data$Class)
  
  # Sauvegarde du dendrogramme
  png(paste0("plots/partie_1/CAH_sur_k_premieres_composantes/dendro_k", k, ".png"))
  plot(cah_k, main = paste("Dendrogramme sur", k, "composantes principales"), labels = FALSE)
  rect.hclust(cah_k, k = 2, border = "darkgreen")
  dev.off()
  
  return(c(Taux_Erreur = error, ARI = ari))
}

# Appliquer la fonction pour k = 1 à 7
results_list <- lapply(1:7, compute_cah_metrics)

# Transformer la liste en data.frame
results_mat <- do.call(rbind, results_list)
error_df <- data.frame(Nb_CP = 1:7, results_mat)
error_df$Taux_Erreur <- round(error_df$Taux_Erreur, 4)
error_df$ARI <- round(error_df$ARI, 4)

# Afficher le tableau
print(error_df)

# Sauvegarde graphique
library(ggplot2)
p_err <- ggplot(error_df, aes(x = Nb_CP)) +
  geom_line(aes(y = Taux_Erreur), color = "red") +
  geom_point(aes(y = Taux_Erreur), color = "red") +
  ylab("Taux d'erreur de classification") +
  xlab("Nombre de composantes principales") +
  ggtitle("Taux d'erreur en fonction du nombre de CP")

ggsave("plots/partie_1/CAH_sur_k_premieres_composantes/taux_erreur_par_k.png", plot = p_err)

# Meilleur k
best_k <- which.min(error_df$Taux_Erreur)
cat("Le nombre de composantes principales minimisant l'erreur est :", best_k, 
    "avec un taux d'erreur de", error_df$Taux_Erreur[best_k], "\n")
cat("Indice ARI correspondant :", error_df$ARI[best_k], "\n")

