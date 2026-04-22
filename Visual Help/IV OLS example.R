library(AER)
library(MASS)

set.seed(123)
n <- 5000

# Instrument exogène
Z <- rnorm(n)

# Simule erreurs corrélées pour X et Y
Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
err <- mvrnorm(n = n, mu = c(0,0), Sigma = Sigma)
u <- err[,1]   # erreur Y
v <- err[,2]   # erreur X

# Génération X dépendant de Z et de v (endogénéité via v)
X <- 0.8*Z + v

# Structure Y vraie
beta_true <- 2.5
Y <- beta_true*X + u



plot(X, u,
     main="Endogénéité : corr(X,u) ≠ 0",
     xlab="X", ylab="u")
abline(lm(u ~ X), col="red")
#E[Xu]=/0 donc endogénéité et exogénéité excluse


ols <- lm(Y ~ X)
summary(ols)


plot(Z, X,
     main="Pertinence : Z explique X",
     xlab="Z", ylab="X")
abline(lm(X ~ Z), col="blue")
#On voit que l'instrument permet d'expliquer la variable indépendante (qui je le rappelle est dépendante avec les erreurs u)

plot(Z, u,
     main="Exogénéité : Z indépendant de u",
     xlab="Z", ylab="u")
abline(lm(u ~ Z), col="red")
#Et que Z soit exogène avec u (sinon même problème qu'endogénéité de X avec u)

stage1 <- lm(X ~ Z)
summary(stage1)
X_hat <- fitted(stage1)
plot(X, X_hat,
     main="X observé vs X prédit par Z",
     xlab="X", ylab="X_hat")
abline(0,1,col="red")


stage2 <- lm(Y ~ X_hat)
summary(stage2)


ols <- lm(Y ~ X)
iv  <- ivreg(Y ~ X | Z)

coef_ols <- coef(ols)["X"]
coef_iv  <- coef(iv)["X"]

estimates <- c(coef_ols, coef_iv)
names(estimates) <- c("OLS", "IV")

barplot(estimates,
        ylim = c(0, max(estimates, 3.5)),
        col = c("red", "blue"),
        main = "Comparaison OLS vs IV",
        ylab = "Coefficient estimé")

abline(h = 2.5, lwd = 3, lty = 2)
legend("topleft",
       legend = c("OLS (biaisé)", "IV", "Vraie valeur"),
       col = c("red", "blue", "black"),
       lwd = 2.5,
       lty = c(1,1,2))


B <- 500
beta_ols <- numeric(B)
beta_iv  <- numeric(B)

for (b in 1:B) {
  u <- rnorm(n)
  Z <- rbinom(n, 1, 0.5)
  X <- 1.5*Z + 2*u + rnorm(n)
  Y <- 3*X + u
  
  beta_ols[b] <- coef(lm(Y ~ X))["X"]
  beta_iv[b]  <- coef(ivreg(Y ~ X | Z))["X"]
}


x_min <- 2.9
x_max <- 3.4

hist(beta_ols,
     breaks = 30,
     col = rgb(1,0,0,0.4),
     freq = FALSE,
     xlim = c(x_min, x_max),
     main = "Distribution des estimateurs OLS vs IV",
     xlab = "Coefficient estimé")

hist(beta_iv,
     breaks = 30,
     col = rgb(0,0,1,0.4),
     freq = FALSE,
     add = TRUE)

abline(v = 3, lwd = 3, lty = 2)

legend("topright",
       legend = c("OLS", "IV", "Vraie valeur"),
       fill = c(rgb(1,0,0,0.4), rgb(0,0,1,0.4), NA),
       border = NA)


bias_ols <- mean(beta_ols) - 3
bias_iv  <- mean(beta_iv)  - 3

sd_ols <- sd(beta_ols)
sd_iv  <- sd(beta_iv)

bias_var <- rbind(
  OLS = c(bias_ols, sd_ols),
  IV  = c(bias_iv,  sd_iv)
)

colnames(bias_var) <- c("Biais", "Ecart-type")

barplot(t(bias_var),
        beside = TRUE,
        col = c("darkred","darkblue"),
        main = "Biais et variance : OLS vs IV",
        ylab = "Valeur")

legend("topright",
       legend = c("Biais", "Ecart-type"),
       fill = c("darkred", "darkblue"),
       bty = "n")



