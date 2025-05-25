library(FactoMineR)

dir.create("plots/partie_3")


# Chargement des données
library(openxlsx)
raisin <- read.xlsx("dataset/Raisin.xlsx", sheet = 1)

set.seed(1)
n <- nrow(raisin)
train <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(2/3, 1/3))
Z_train <- PCA(raisin[train, 1:7], scale.unit = TRUE, graph = FALSE)$ind$coord[, 1:2]
y_train <- raisin$Class[train]

## Question 3-1-a
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
a_hat
b_hat




## Question 3-1-b
eig <- eigen(Sigma_hat)
U <- eig$vectors
D_inv <- diag(1/eig$values)
Sigma_inv_eig <- U %*% D_inv %*% t(U)

a_hat_eig <- Sigma_inv_eig %*% (mu1_hat - mu0_hat)
b_hat_eig <- as.numeric(0.5 * (
  t(mu1_hat) %*% Sigma_inv_eig %*% mu1_hat -
    t(mu0_hat) %*% Sigma_inv_eig %*% mu0_hat
))

a_hat_eig
b_hat_eig

# Vérification de l'égalité
if (all.equal(b_hat, b_hat_eig, tolerance = 1e-10) == TRUE) {
  print("OK")
} else {
  print("Différence détectée")
  print(c(b_hat = b_hat, b_hat_eig = b_hat_eig))
}






## Question 3-1-c
plot(Z_train[, 1], Z_train[, 2], col = ifelse(y_train == "Kecimen", "blue", "red"), 
     xlab = "Dim.1", ylab = "Dim.2", pch = 16)
abline(a = b_hat / a_hat[2], b = -a_hat[1] / a_hat[2], col = "purple")
legend("topright", legend = c("Kecimen", "Besni"), col = c("blue", "red"), pch = 16)


## Question 3-1-d
raisin_pca <- PCA(raisin[train, 1:7], scale.unit = TRUE, graph = FALSE)
train <- sample(1:nrow(raisin), 0.7 * nrow(raisin))
test <- setdiff(1:nrow(raisin), train)

Z_test <- predict(raisin_pca, newdata = raisin[test, 1:7])$coord[, 1:2]
y_test <- raisin$Class[test]

pred <- Z_test %*% a_hat > b_hat
y_pred <- ifelse(pred, "Besni", "Kecimen")
error_rate <- mean(y_pred != y_test)
error_rate

# Vérification en utilisant Ida du package MASS
library(MASS)
lda_model <- lda(Z_train, grouping = y_train)
pred_lda <- predict(lda_model, newdata = Z_test)$class
error_rate_lda <- mean(pred_lda != y_test)
error_rate_lda


# QDA vs LDA ?
library(biotools)  # pour le test de Box

# Supposons que ta data s'appelle raisin, avec une colonne "Class"
# On commence par vérifier la covariance par classe

# Extraire les classes
classes <- unique(raisin$Class)

# Extraire les colonnes numériques uniquement
numeric_cols <- sapply(raisin, is.numeric)
vars_numeric <- names(raisin)[numeric_cols]

# Calcul des matrices de covariance par classe sur les colonnes numériques
cov_list <- lapply(classes, function(cl) {
  cov(raisin[raisin$Class == cl, vars_numeric])
})
names(cov_list) <- classes

print(cov_list)

# Appliquer le test de Box uniquement sur les variables numériques
box_test <- boxM(raisin[, vars_numeric], raisin$Class)
print(box_test)

box_test$p.value # < 0.05

# Question 3

# Split data into training and test sets (2/3 training, 1/3 test)
n <- nrow(raisin)
train <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(2/3, 1/3))
X_train <- raisin[train, 1:7]  # All original variables
y_train <- raisin$Class[train]
X_test <- raisin[!train, 1:7]
y_test <- raisin$Class[!train]

# Standardize the explanatory variables
X_train_scaled <- scale(X_train, center = TRUE, scale = TRUE)
X_test_scaled <- scale(X_test, center = attr(X_train_scaled, "scaled:center"), 
                       scale = attr(X_train_scaled, "scaled:scale"))

# Fit LDA model on all original variables
lda_full <- lda(X_train_scaled, grouping = y_train)

# Predict on test set
pred_lda_full <- predict(lda_full, newdata = X_test_scaled)
y_pred_full <- pred_lda_full$class
error_rate_full <- mean(y_pred_full != y_test)
cat("Test error rate for LDA (full variables):", error_rate_full, "\n")
