
library(plm)

library(reshape2)

set.seed(123)  # pour reproductibilité

# Paramètres
n_indiv <- 50
n_years <- 10
years <- 2010:2019

# Création du panel
id <- rep(1:n_indiv, each = n_years)
year <- rep(years, times = n_indiv)

set.seed(123)

n_indiv <- 50
n_years <- 10
years <- 2010:2019

# Identifiants
id <- rep(1:n_indiv, each = n_years)
year <- rep(years, times = n_indiv)

# Variables explicatives
x1 <- rnorm(n_indiv * n_years, mean = 1500, sd = 100)
x2 <- rnorm(n_indiv * n_years, mean = 10, sd = 3)

# Effets individuels très forts pour créer un biais OLS
alpha_i <- rnorm(n_indiv, mean = 0, sd = 300)   # gros effet fixe individuel
gamma_t <- rnorm(n_years, mean = 0, sd = 50)   # gros effet temporel

# Variable dépendante avec effet fixe et bruit
y <- 1 + 0.5*x1 + 2*x2 + rep(alpha_i, each = n_years) + rep(gamma_t, times = n_indiv) + rnorm(n_indiv*n_years, 0, 1)

# Data.frame final
panel_df <- data.frame(id = id, year = year, y = y, x1 = x1, x2 = x2)

# Aperçu
head(panel_df)

pdata <- pdata.frame(panel_df, index = c("id", "year"))

fe_model <- plm(y ~ x1 + x2, data = pdata, model = "within")
summary(fe_model)

re_model <- plm(y ~ x1 + x2, data = pdata, model = "random")
summary(re_model)

# Modèle à effets fixes sur individus et années
twfe_model <- plm(y ~ x1 + x2, data = pdata, model = "within", effect = "twoways")
summary(twfe_model)


# Effets fixes individuels
fe_ind <- fixef(twfe_model, effect = "individual")

# Effets fixes temporels
fe_time <- fixef(twfe_model, effect = "time")



library(ggplot2)

# Effets fixes individuels
df_ind <- data.frame(id = names(fe_ind), fe = as.numeric(fe_ind))
ggplot(df_ind, aes(x = as.factor(id), y = fe)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Effets fixes individuels", x = "Individu", y = "Effet fixe") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # cacher les id si trop nombreux

# Effets fixes temporels
df_time <- data.frame(year = as.numeric(names(fe_time)), fe = as.numeric(fe_time))
ggplot(df_time, aes(x = year, y = fe)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Effets fixes temporels", x = "Année", y = "Effet fixe") +
  theme_minimal()

# Ajouter effets fixes individu et temporel à la df
panel_df$fe_ind <- rep(fe_ind, each = n_years)
panel_df$fe_time <- rep(fe_time, times = n_indiv)

# Effet total fixe (individuel + temporel)
panel_df$fe_total <- panel_df$fe_ind + panel_df$fe_time

# Graphique combiné
ggplot(panel_df, aes(x = year, y = fe_total, group = as.factor(id), color = as.factor(id))) +
  geom_line(alpha = 0.7) +
  labs(title = "Effets fixes combinés (individus + années)",
       x = "Année", y = "Effet fixe total", color = "Individu") +
  theme_minimal() +
  theme(legend.position = "none")  # cacher la légende si trop d'individus

# Variable dépendante ajustée : y - effets fixes
panel_df$y_twfe <- panel_df$y - panel_df$fe_total

ggplot(panel_df, aes(x = year, group = as.factor(id))) +
  geom_line(aes(y = y, color = "Original"), alpha = 0.5) +
  geom_line(aes(y = y_twfe, color = "Après TWFE"), size = 1) +
  labs(title = "Effet du TWFE sur la variable dépendante",
       x = "Année", y = "Valeur de y",
       color = "Lignes") +
  theme_minimal()

ols_model <- lm(y ~ x1 + x2, data = panel_df)
summary(ols_model)

library(plm)
pdata <- pdata.frame(panel_df, index = c("id", "year"))

twfe_model <- plm(y ~ x1 + x2, data = pdata, model = "within", effect = "twoways")
summary(twfe_model)
# Coefficients
beta_ols <- coef(ols_model)
beta_twfe <- coef(twfe_model)

# Ne garder que x1 et x2 pour la comparaison
beta_ols_no_intercept <- beta_ols[-1]  # enlever le (Intercept)

comparison <- data.frame(
  Variable = names(beta_ols_no_intercept),
  Beta_OLS = beta_ols_no_intercept,
  Beta_TWFE = beta_twfe
)

comparison


library(ggplot2)

comparison_melt <- reshape2::melt(comparison, id.vars = "Variable")

ggplot(comparison_melt, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison des coefficients : OLS vs TWFE",
       x = "Variable", y = "Coefficient", fill = "Modèle") +
  theme_minimal()

