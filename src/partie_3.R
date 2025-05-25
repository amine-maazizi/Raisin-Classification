library(readxl)
library(FactoMineR)



raisin = read_excel("C:/Users/boufo/Documents/Raisin-Classification/dataset/Raisin.xlsx")

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
a_hat_eig <- U %*% D_inv %*% t(U) %*% (mu1_hat - mu0_hat)

a_hat_eig




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




