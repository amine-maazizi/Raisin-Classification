# Effacer l'espace de travail et fermer les graphiques
rm(list = objects())
graphics.off()

# Créer les répertoires nécessaires s'ils n'existent pas
dir.create("plots/partie_1", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/partie_1/analyse_uni_et_bi_variee", showWarnings = FALSE)
dir.create("plots/partie_1/ACP", showWarnings = FALSE)
dir.create("plots/partie_1/CAH", showWarnings = FALSE)
dir.create("plots/partie_1/CAH_sur_k_premieres_composantes", showWarnings = FALSE)

# Charger les packages nécessaires
library(openxlsx)
library(ggplot2)
library(reshape2)
library(corrplot)
library(GGally)
library(FactoMineR)
library(cowplot)
library(factoextra)
library(cluster)
library(mclust)

# Charger les données
donnees <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)

# Dimensions des données
n <- nrow(donnees)  # Nombre d'observations
p <- ncol(donnees)  # Nombre de variables

# Vérifier les valeurs manquantes
colSums(is.na(donnees))

# Renommer les colonnes en français
colnames(donnees) <- c("Surface", "Périmètre", "LongueurAxeMajeur", "LongueurAxeMineur", 
                       "Excentricité", "SurfaceConvexe", "Étendue", "Classe")

# Liste des variables morphologiques
variables_morph <- c("Surface", "Périmètre", "LongueurAxeMajeur", "LongueurAxeMineur", 
                     "Excentricité", "SurfaceConvexe", "Étendue")

# ========================================
# ANALYSE DES DONNÉES (UNIVARIÉE ET BIVARIÉE)
# ========================================
# Statistiques descriptives
stats <- data.frame(
  Variable = variables_morph,
  Minimum = sapply(variables_morph, function(var) min(donnees[[var]], na.rm = TRUE)),
  Maximum = sapply(variables_morph, function(var) max(donnees[[var]], na.rm = TRUE)),
  Moyenne = sapply(variables_morph, function(var) mean(donnees[[var]], na.rm = TRUE))
)
cat("Statistiques descriptives des variables morphologiques :\n")
stats[, c("Minimum", "Maximum", "Moyenne")] <- round(stats[, c("Minimum", "Maximum", "Moyenne")], 3)
print(stats)
cat("\n")

# Boxplots facettés par classe
donnees_mod <- melt(donnees, id.vars = "Classe", measure.vars = 1:(p - 1))
p0 <- ggplot(donnees_mod, aes(x = as.factor(Classe), y = value, fill = as.factor(Classe))) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  xlab("Classe") + ylab("Valeur") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(fill = "Classe") +
  theme(legend.position = "top")
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_toutes_variables_par_classe.png", plot = p0)

# Matrice de corrélation
round(cor(donnees[, -p]), 2)

# Visualisation avec corrplot
png("plots/partie_1/analyse_uni_et_bi_variee/corrplot.png")
corrplot(cor(donnees[, -p]), method = "circle")
dev.off()

# Scatter plots bivariés avec GGpairs
p1 <- ggpairs(donnees, ggplot2::aes(colour = as.factor(Classe)))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/ggpairs_par_classe.png", plot = p1)

# ========================================
# ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
# ========================================
vecteur_classes <- donnees$Classe
res_acp <- PCA(donnees[, -p], scale.unit = TRUE, graph = FALSE, ncp = 7)

# Éboulis des valeurs propres
p1 <- fviz_eig(res_acp, addlabels = TRUE, main = "Éboulis des valeurs propres") +
  geom_hline(yintercept = 100 / p)
ggsave("plots/partie_1/ACP/eboulis_valeurs_propres.jpg", plot = p1)

# Cercles de corrélation pour les plans 1-2 et 2-3
p2 <- fviz_pca_var(res_acp, axes = c(1, 2), repel = TRUE)
p3 <- fviz_pca_var(res_acp, axes = c(2, 3), repel = TRUE)
ggsave("plots/partie_1/ACP/cercle_correlation_1_2.jpg", plot = p2)
ggsave("plots/partie_1/ACP/cercle_correlation_2_3.jpg", plot = p3)

# Projection des individus sur les plans 1-2 et 2-3
p_ind_1_2 <- fviz_pca_ind(res_acp,
                          habillage = as.factor(vecteur_classes),
                          addEllipses = TRUE,
                          label = "none",
                          repel = TRUE
)
ggsave("plots/partie_1/ACP/projection_individus_plan1_2.jpg", plot = p_ind_1_2, width = 12, height = 6)

p_ind_2_3 <- fviz_pca_ind(res_acp,
                          habillage = as.factor(vecteur_classes),
                          axes = c(2, 3),
                          addEllipses = TRUE,
                          label = "none",
                          repel = TRUE
)
ggsave("plots/partie_1/ACP/projection_individus_plan_2_3.jpg", plot = p_ind_2_3, width = 12, height = 6)

# ========================================
# CLASSIFICATION ASCENDANTE HIÉRARCHIQUE (CAH)
# ========================================
# Données standardisées
donnees_standardisees <- scale(donnees[, -p])
dist_standardisees <- dist(donnees_standardisees, method = "euclidean")
cah_standardisees <- hclust(dist_standardisees, method = "ward.D2")

# Dendrogramme standardisé
png("plots/partie_1/CAH/dendrogramme_standardise.png", width = 800, height = 600)
plot(cah_standardisees, main = "CAH avec standardisation", labels = FALSE)
rect.hclust(cah_standardisees, k = 2, border = "blue")
dev.off()

# Hauteur des fusions
png("plots/partie_1/CAH/hauteurs_fusions.png", width = 800, height = 600)
plot(rev(cah_standardisees$height), type = "b", pch = 19,
     xlab = "Fusion (ordre inverse)", ylab = "Hauteur",
     main = "Hauteur des fusions (Ward)"
)
dev.off()

# Choix du nombre optimal de clusters
png("plots/partie_1/CAH/silhouette_nb_clusters.png", width = 800, height = 600)
fviz_nbclust(donnees_standardisees, FUN = hcut, method = "silhouette") +
  ggtitle("Nombre optimal de clusters - Méthode silhouette (Ward)")
dev.off()

# Silhouette pour k = 2
grp_standardisees <- cutree(cah_standardisees, k = 2)
sil_standardisees <- silhouette(grp_standardisees, dist_standardisees)
p_sil_standardisees <- fviz_silhouette(sil_standardisees, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_standardise.jpg", plot = p_sil_standardisees)

sil_moyenne_standardisees <- mean(sil_standardisees[, 3])
cat("Indice de silhouette moyen (k=2, standardisé) :", round(sil_moyenne_standardisees, 3), "\n")

# ARI et matrice de confusion
ari_standardisees <- adjustedRandIndex(grp_standardisees, donnees$Classe)
cat("ARI avec standardisation :", round(ari_standardisees, 3), "\n")

conf_matrix_standardisees <- table(donnees$Classe, grp_standardisees)
taux_erreur_standardisees <- 1 - sum(diag(conf_matrix_standardisees)) / sum(conf_matrix_standardisees)
cat("Taux d'erreur de classification (k=2, standardisé) :", round(taux_erreur_standardisees, 3), "\n")

# Données brutes (non standardisées)
donnees_brutes <- donnees[, -p]
dist_brutes <- dist(donnees_brutes, method = "euclidean")
cah_brutes <- hclust(dist_brutes, method = "ward.D2")

# Dendrogramme brut
png("plots/partie_1/CAH/dendrogramme_brut.png", width = 800, height = 600)
plot(cah_brutes, main = "CAH sans standardisation", labels = FALSE)
rect.hclust(cah_brutes, k = 2, border = "red")
dev.off()

# Silhouette pour k = 2
grp_brutes <- cutree(cah_brutes, k = 2)
sil_brutes <- silhouette(grp_brutes, dist_brutes)
p_sil_brutes <- fviz_silhouette(sil_brutes, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_brut.jpg", plot = p_sil_brutes)

sil_moyenne_brutes <- mean(sil_brutes[, 3])
cat("Indice de silhouette moyen (k=2, brut) :", round(sil_moyenne_brutes, 3), "\n")

# ARI et matrice de confusion
ari_brutes <- adjustedRandIndex(grp_brutes, donnees$Classe)
cat("ARI sans standardisation :", round(ari_brutes, 3), "\n")

conf_matrix_brutes <- table(donnees$Classe, grp_brutes)
taux_erreur_brutes <- 1 - sum(diag(conf_matrix_brutes)) / sum(conf_matrix_brutes)
cat("Taux d'erreur de classification (k=2, brut) :", round(taux_erreur_brutes, 3), "\n")

# ========================================
# CAH SUR LES COMPOSANTES PRINCIPALES
# ========================================
compute_cah_metrics <- function(k) {
  pc_k <- res_acp$ind$coord[, 1:k]
  dist_k <- dist(pc_k)
  cah_k <- hclust(dist_k, method = "ward.D2")
  grp_k <- cutree(cah_k, k = 2)
  
  # Matrice de confusion
  conf_mat <- table(donnees$Classe, grp_k)
  erreur <- 1 - sum(diag(conf_mat)) / sum(conf_mat)
  ari <- adjustedRandIndex(grp_k, donnees$Classe)
  
  # Sauvegarde du dendrogramme
  png(paste0("plots/partie_1/CAH_sur_k_premieres_composantes/dendro_k", k, ".png"))
  plot(cah_k, main = paste("Dendrogramme sur", k, "composantes principales"), labels = FALSE)
  rect.hclust(cah_k, k = 2, border = "darkgreen")
  dev.off()
  
  return(c(Taux_Erreur = erreur, ARI = ari))
}

# Appliquer la fonction pour k = 1 à 7
resultats_liste <- lapply(1:7, compute_cah_metrics)

# Transformer la liste en data.frame
resultats_mat <- do.call(rbind, resultats_liste)
df_erreur <- data.frame(Nb_CP = 1:7, resultats_mat)
df_erreur$Taux_Erreur <- round(df_erreur$Taux_Erreur, 4)
df_erreur$ARI <- round(df_erreur$ARI, 4)

# Afficher le tableau
print(df_erreur)

# Sauvegarde graphique
p_err <- ggplot(df_erreur, aes(x = Nb_CP)) +
  geom_line(aes(y = Taux_Erreur), color = "red") +
  geom_point(aes(y = Taux_Erreur), color = "red") +
  ylab("Taux d'erreur de classification") +
  xlab("Nombre de composantes principales") +
  ggtitle("Taux d'erreur en fonction du nombre de CP")
ggsave("plots/partie_1/CAH_sur_k_premieres_composantes/taux_erreur_par_k.png", plot = p_err)

# Meilleur k
meilleur_k <- which.min(df_erreur$Taux_Erreur)
cat("Le nombre de composantes principales minimisant l'erreur est :", meilleur_k,
    "avec un taux d'erreur de", df_erreur$Taux_Erreur[meilleur_k], "\n")
cat("Indice ARI correspondant :", df_erreur$ARI[meilleur_k], "\n")