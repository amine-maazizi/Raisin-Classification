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
res <- PCA(data[, -p], scale.unit = TRUE, graph = FALSE)

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



# CAH
library(cluster)
library(mclust)
library(ggplot2)
library(ggdendro)

# Données
data_quant <- data[,-p]
variety <- as.factor(data$Class)

# CAH avec méthode de Ward
res_cah <- hclust(dist(data_quant), method = "ward.D2")

# Récupération des sauts d'inertie (delta_t = hauteur des fusions)
delta_t <- diff(res_cah$height)

# Tracé du diagramme en bâtons inversé des sauts d'inertie
df_delta <- data.frame(etape = 1:length(delta_t), delta = rev(delta_t))

gg_delta <- ggplot(df_delta, aes(x = etape, y = delta)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Diagramme des sauts d'inertie (méthode du coude)",
       x = "Étapes de fusion (du dernier au premier)", y = expression(delta[t])) +
  theme_minimal()

ggsave("plots/partie_1/CAH/diagramme_sauts_inertie.jpg", gg_delta,
       width = 8, height = 5, dpi = 300)

print(gg_delta)

# Optionnel : Affichage du dendrogramme (Ward)
dendro_data <- dendro_data(res_cah)
gg_dendro <- ggplot() +
  geom_segment(data = dendro_data$segments, 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_minimal() +
  labs(title = "Dendrogramme CAH (méthode Ward)", x = "", y = "Distance")
ggsave("plots/partie_1/CAH/dendrogramme_ward.jpg", gg_dendro,
       width = 10, height = 6, dpi = 300)
print(gg_dendro)

sil <- silhouette(cutree(res_cah, k = 2), dist(data_quant))
p_sil <- fviz_silhouette(sil, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_complete.jpg", plot = p_sil)
sil_mean <- mean(sil[,3])
cat("Indice de silhouette moyen (k=2) :", sil_mean, "\n")

grp_k2 <- cutree(res_cah, k = 2)
conf_matrix <- table(variety, grp_k2)
error_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Erreur de classification (k=2) :", error_rate, "\n")

data_quant_scaled <- scale(data_quant)
res_cah_scaled <- hclust(dist(data_quant_scaled), method = "complete")

sil_scaled <- silhouette(cutree(res_cah_scaled, k = 2), dist(data_quant_scaled))
p_sil_scaled <- fviz_silhouette(sil_scaled, print.summary = FALSE)
ggsave("plots/partie_1/CAH/silhouette_k2_scaled.jpg", plot = p_sil_scaled)
sil_mean_scaled <- mean(sil_scaled[,3])
cat("Indice de silhouette moyen (k=2, normalisé) :", sil_mean_scaled, "\n")

grp_k2_scaled <- cutree(res_cah_scaled, k = 2)
conf_matrix_scaled <- table(variety, grp_k2_scaled)
error_rate_scaled <- 1 - sum(diag(conf_matrix_scaled)) / sum(conf_matrix_scaled)
cat("Erreur de classification (k=2, normalisé) :", error_rate_scaled, "\n")


# CAH sur les k premières composantes pour classification
PC_scores <- res$ind$coord
variety <- as.factor(data$Class)[rownames(PC_scores)]

evaluate_error <- function(k) {
  data_k <- PC_scores[, 1:k]
  dist_k <- dist(data_k)
  cah_k <- hclust(dist_k, method = "complete")
  groupes_k <- cutree(cah_k, k = 2)
  error <- 1 - sum(diag(table(variety, groupes_k))) / length(variety)
  return(error)
}

error_rates <- sapply(1:5, evaluate_error)

print(round(error_rates, 4))

plot(1:5, error_rates, type = "b", xlab = "Nombre d'axes PCA", ylab = "Erreur de classification",
     main = "Erreur de classification selon le nb d'axes PCA utilisés")

best_k <- which.min(error_rates)
cat("Meilleur nombre d'axes PCA =", best_k, "\n")

# Save this final plot
png("plots/partie_1/CAH_sur_k_premieres_composantes/error_vs_k_axes.png")
plot(1:5, error_rates, type = "b", xlab = "Nombre d'axes PCA", ylab = "Erreur de classification",
     main = "Erreur de classification selon le nb d'axes PCA utilisés")
dev.off()