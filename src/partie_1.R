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

library(ggplot2)
library(reshape2)
library(corrplot)
library(car)
library(GGally)

# Analyse univariée et bivariée
# Boxplot toutes variables
png("plots/partie_1/analyse_uni_et_bi_variee/boxplot_all.png")
boxplot(data[,-p])
dev.off()

# Boxplot Area
p1 <- ggplot(data) + geom_boxplot(aes(y=Area))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_area.png", plot = p1)

# Boxplot Area par Class
p2 <- ggplot(data) +
  geom_boxplot(aes(x=as.factor(Class), y=Area, col=as.factor(Class))) +
  xlab("Class") + ylab("Area")
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_area_class.png", plot = p2)

# Boxplots multiples
data_mod <- melt(data, id.vars="Class", measure.vars=1:(p-1))
p3 <- ggplot(data_mod) +
  geom_boxplot(aes(x=variable, y=value, color=variable)) +
  xlab("Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/boxplot_multi.png", plot = p3)

# Variance
round(apply(data[,-p], 2, var), 2)

# Scatterplot matrice
png("plots/partie_1/analyse_uni_et_bi_variee/pairs_plot.png")
pairs(data[,-p])
dev.off()

# Corrélation
round(cor(data[,-p]), 2)

# Corrplot
png("plots/partie_1/analyse_uni_et_bi_variee/corrplot.png")
corrplot(cor(data[,-p]), method="circle")
dev.off()

# Scatterplot matrice (car)
png("plots/partie_1/analyse_uni_et_bi_variee/scatterplot_matrix_car.png")
scatterplotMatrix(as.matrix(data[,-p]))
dev.off()

# GGpairs quantitatives
p4 <- ggpairs(data[,-p])
ggsave("plots/partie_1/analyse_uni_et_bi_variee/ggpairs_quant.png", plot = p4)

# GGpairs avec Class
p5 <- ggpairs(data, ggplot2::aes(colour=as.factor(Class)))
ggsave("plots/partie_1/analyse_uni_et_bi_variee/ggpairs_class.png", plot = p5)


# ACP
library(FactoMineR)
library(cowplot)
library(factoextra)

res <- PCA(data[,-p], scale.unit = TRUE, graph = FALSE)

# a - Valeurs et vecteurs propres
round(res$eig, 4)
sum(res$eig[,1])

p1 <- fviz_eig(res, addlabels = TRUE, main = "Éboulis des valeurs propres") +
  geom_hline(yintercept = 100/p)
ggsave("plots/partie_1/ACP/scree_plot.jpg", plot = p1)

# b - Cercle des corrélations
p2 <- plot(res, choix = "var", axes = c(1,2), graph.type = "classic")
ggsave("plots/partie_1/ACP/cor_circle_1_2.jpg", plot = p2)
p3 <- plot(res, choix = "var", axes = c(2,3), graph.type = "classic")
ggsave("plots/partie_1/ACP/cor_circle_2_3.jpg", plot = p3)

# c - Analyse simultanée individus/variables
p4 <- plot(res, choix = "ind", axes = c(1,2), label = "none", graph.type = "classic")
p5 <- plot(res, choix = "var", axes = c(1,2), graph.type = "classic")
ggsave("plots/partie_1/ACP/ind_var_plan1_2.jpg", plot = plot_grid(p4, p5, ncol = 2))

Ind <- res$ind
indi <- as.numeric(names(head(sort(Ind$contrib[,1], decreasing = TRUE), 7)))
p6 <- plot(res, choix = "ind", axes = c(1,2), label = "none", graph.type = "classic") +
  geom_point(aes(x = Ind$coord[indi,1], y = Ind$coord[indi,2]), col = "red", size = 2) +
  geom_text(aes(x = Ind$coord[indi,1], y = Ind$coord[indi,2]), label = indi, col = 2, nudge_y = 0.1)
ggsave("plots/partie_1/ACP/ind_contrib_plan1_2.jpg", plot = p6)

# Contributions par axe
apply(Ind$contrib, 2, which.max)
head(sort(Ind$contrib[,1], decreasing = TRUE), 7)

# Meilleure représentation axe 2
which.max(Ind$cos2[,2])

# Plan (2,3)
p7 <- plot(res, choix = "ind", axes = c(2,3), label = "none", graph.type = "classic") +
  geom_point(aes(x = Ind$coord[indi,2], y = Ind$coord[indi,3]), col = "red", size = 2) +
  geom_text(aes(x = Ind$coord[indi,2], y = Ind$coord[indi,3]), label = indi, col = 2, nudge_y = 0.1)
p8 <- plot(res, choix = "var", axes = c(2,3), graph.type = "classic")
ggsave("plots/partie_1/ACP/ind_var_plan2_3.jpg", plot = plot_grid(p7, p8))

# Contributif et mal représenté
indj <- which(res$ind$cos2[,2] + res$ind$cos2[,3] < 0.15 & 
                (res$ind$coord[,2]^2 + res$ind$coord[,3]^2) > mean(res$ind$coord[,2]^2 + res$ind$coord[,3]^2))
p9 <- plot(res, choix = "ind", axes = c(2,3), label = "none", graph.type = "classic") +
  geom_point(aes(x = Ind$coord[indj,2], y = Ind$coord[indj,3]), col = "purple", size = 2) +
  geom_text(aes(x = Ind$coord[indj,2], y = Ind$coord[indj,3]), label = indj, col = "purple", nudge_y = 0.1)
ggsave("plots/partie_1/ACP/ind_contrib_mal_plan2_3.jpg", plot = p9)

# Peu contributif et bien représenté
which(res$ind$cos2[,2] > 0.85 & res$ind$contrib[,2] < 0.25)

# Contributif et bien représenté
which(res$ind$cos2[,2] > 0.95 & res$ind$contrib[,2] > 0.85)

# Individus supplémentaires
df <- as.data.frame(lapply(data, function(x) tapply(x, data$Class, mean)))
indsup <- nrow(data) + 1:nrow(df)
data[indsup,] <- df
res_sup <- PCA(data[,-p], ind.sup = indsup, graph = FALSE)
p10 <- plot(res_sup, choix = "ind", axes = c(1,2), label = "ind.sup", graph.type = "classic")
p11 <- plot(res_sup, choix = "ind", axes = c(2,3), label = "ind.sup", graph.type = "classic")
ggsave("plots/partie_1/ACP/ind_sup_plan1_2.jpg", plot = p10)
ggsave("plots/partie_1/ACP/ind_sup_plan2_3.jpg", plot = p11)

# Variable qualitative Class
data$Class <- as.factor(data$Class)
res_qual <- PCA(data, quali.sup = p, graph = FALSE)

# Graphique avec ggplot2
p12_data <- data.frame(
  x = res_qual$ind$coord[,1],
  y = res_qual$ind$coord[,2],
  Class = data$Class[1:nrow(res_qual$ind$coord)]
)
p12_supp <- data.frame(
  x = res_qual$quali.sup$coord[,1],
  y = res_qual$quali.sup$coord[,2]
)
p12 <- ggplot() +
  geom_point(data = p12_data, aes(x = x, y = y, color = Class)) +
  geom_point(data = p12_supp, aes(x = x, y = y), color = "blue", size = 3) +
  labs(x = "Dim 1", y = "Dim 2", title = "Plan (1,2) avec Class")
ggsave("plots/partie_1/ACP/ind_quali_plan1_2.jpg", plot = p12)

p13_data <- data.frame(
  x = res_qual$ind$coord[,2],
  y = res_qual$ind$coord[,3],
  Class = data$Class[1:nrow(res_qual$ind$coord)]
)
p13_supp <- data.frame(
  x = res_qual$quali.sup$coord[,2],
  y = res_qual$quali.sup$coord[,3]
)
p13 <- ggplot() +
  geom_point(data = p13_data, aes(x = x, y = y, color = Class)) +
  geom_point(data = p13_supp, aes(x = x, y = y), color = "blue", size = 3) +
  labs(x = "Dim 2", y = "Dim 3", title = "Plan (2,3) avec Class")
ggsave("plots/partie_1/ACP/ind_quali_plan2_3.jpg", plot = p13)


# CAH
library(cluster)
library(mclust)

data_quant <- data[,-p]
variety <- as.factor(data$Class)

res_cah <- hclust(dist(data_quant), method = "complete")

jpeg("plots/partie_1/CAH/dendrogram_complete.jpg")
plot(res_cah, cex = 0.5, main = "Dendrogramme CAH (complete)")
abline(h = 15000)
dev.off()

grp_h15000 <- cutree(res_cah, h = 15000)
nb_groups <- length(unique(grp_h15000))
cat("Nombre de groupes à h=15000 :", nb_groups, "\n")

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