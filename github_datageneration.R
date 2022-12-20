int.corr <- diag(6)
int.corr[int.corr!=1] <-.7;
colnames(int.corr) <- paste0("x", 1:dim)

set.seed(12345)
Z <- MASS::mvrnorm(10^4, rep(0,6), Sigma=int.corr)

X1star <- ifelse(Z[, 1] < 0, 0.1039*Z[, 1] -0.6216 , 1.6619*Z[, 1] -0.6216)
X2star <- ifelse(Z[, 2] < 0, 0.1039*Z[, 2] -0.6216 , 1.6619*Z[, 2] -0.6216)
X3star <- ifelse(Z[, 3] < 0, 0.1039*Z[, 3] -0.6216 , 1.6619*Z[, 3] -0.6216)

X4star <- ifelse(Z[, 4] > 0, 0.1039*Z[, 4] +0.6216 , 1.6619*Z[, 4] +0.6216)
X5star <- ifelse(Z[, 5] > 0, 0.1039*Z[, 5] +0.6216 , 1.6619*Z[, 5] +0.6216)
X6star <- ifelse(Z[, 6] > 0, 0.1039*Z[, 6] +0.6216 , 1.6619*Z[, 6] +0.6216)

Xstar <- data.frame(X1star, X2star, X3star, X4star, X5star, X6star)

write.table(Xstar, "Xstar.csv", row.names=F)
