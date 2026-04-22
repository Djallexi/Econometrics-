
library(ggplot2)
library(reshape2)

set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)

# Histogramme des données
hist(x, breaks=15, col="lightblue",
     main="Histogramme des données",
     xlab="x", ylab="Fréquence")
abline(v=mean(x), col="red", lty=2)  # moyenne empirique

#On voit que ca ressemble à loi normale donc on choisit la loi normale pour faire la vraisemblance
#Vraisemblance qui est le produit de la fonction de densité à chaque x
#On utilise la log vraisemblance plutot que la vraisemblance car c'est plus facilement solvable et différentiable
#On veut donc maximiser le log vraisemblance selon la moyenne et l'écart type

logLikNorm <- function(params, data) {
  mu <- params[1]
  sigma <- params[2]
  if(sigma <= 0) return(-Inf)  # sigma > 0
  n <- length(data)
  ll <- -n/2*log(2*pi) - n*log(sigma) - sum((data - mu)^2)/(2*sigma^2)
  return(ll)
}

#Ici on derive le log vraisemblance par rapport à mu (moyenne)

mu_vals <- seq(3, 7, length=100)
ll_mu <- sapply(mu_vals, function(mu) logLikNorm(c(mu, 2), x))

plot(mu_vals, ll_mu, type="l", col="blue", lwd=2,
     main="Log-vraisemblance en fonction de mu",
     xlab="mu", ylab="log-vraisemblance")
abline(v=mean(x), col="red", lty=2)  # maximum

#Ici on derive le log vraisemblance par rapport à sigma (écart-type)

sigma_vals <- seq(0.5, 4, length=100)
ll_sigma <- sapply(sigma_vals, function(s) logLikNorm(c(mean(x), s), x))

plot(sigma_vals, ll_sigma, type="l", col="darkgreen", lwd=2,
     main="Log-vraisemblance en fonction de sigma",
     xlab="sigma", ylab="log-vraisemblance")
abline(v=sqrt(mean((x-mean(x))^2)), col="red", lty=2)  # maximum

#Maximisation numérique (L-BDGS-B) car analytique serait trop compliquée

res <- optim(
  par=c(mu=0, sigma=1),           # point de départ
  fn=function(p) -logLikNorm(p, x),  # on minimise -logLik
  method="L-BFGS-B",
  lower=c(-Inf, 1e-6)             # sigma > 0
)
res$par

mu_hat    <- res$par[1]
sigma_hat <- res$par[2]

#Comparaison des estimateurs trouvés par rapport aux valeurs qu'on avait fixé avec rnorm
#On peut voir que c'est relativement proche
cat("Moyenne estimée:", mu_hat, " vs valeur réelle:", 2, "\n")
cat("Sigma estimé:", sigma_hat, " vs valeur réelle:", 5, "\n")



