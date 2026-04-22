library(tidyverse)
library(plm)
library(ggplot2)

set.seed(123)

N <- 40
Tt <- 5

library(tidyverse)
library(plm)
library(ggplot2)

set.seed(123)

# Paramètres
N <- 40
Tt <- 5


panel <- panel %>%
  mutate(
    group = ifelse(id <= 20, "Early", "Late"),
    # D=1 seulement l'année du traitement
    D_year = case_when(
      group == "Early" & year > 2 ~ 1,
      group == "Late"  & year > 4 ~ 1,
      TRUE ~ 0
    ),
    # Effet hétérogène appliqué uniquement cette année
    beta_i = case_when(
      D_year == 1 & group == "Early" ~ 75,  # Early: +50 à l'année du traitement
      D_year == 1 & group == "Late"  ~ 25,  # Late: +50 à l'année du traitement
      TRUE ~ 0
    ),
    u = rnorm(nrow(panel), 0, 5),
    CA = 100 + rep(alpha_i, each = Tt) + rep(gamma_t, times = N) + beta_i + u
  )






ggplot(panel, aes(x=year, y=CA, group=id, color=factor(D))) +
  geom_line(alpha=0.5) +
  labs(title="CA des entreprises avec effet traitement hétérogène",
       x="Année", y="CA",
       color="Politique 4 jours") +
  theme_minimal()


pdata <- pdata.frame(panel, index=c("id","year"))
twfe <- plm(CA ~ D, data=pdata, model="within", effect="twoways")
summary(twfe)

panel %>%
  group_by(group, year) %>%
  summarise(mean_CA = mean(CA), .groups="drop") %>%
  ggplot(aes(x=year, y=mean_CA, color=group)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  labs(title="Trajectoires moyennes CA : Early vs Late",
       subtitle="TWFE va mélanger ces comparaisons",
       y="Moyenne CA") +
  theme_minimal()

panel <- panel %>%
  mutate(
    post = ifelse((group == "Early" & year >= 2) | (group == "Late" & year >= 4), 1, 0),
    treat = ifelse(group == "Early", 1, 0)  # Early = groupe traité, Late = contrôle
  )

did_model <- lm(CA ~ treat * post, data = panel)
summary(did_model)

coef_did  <- coef(did_model)["treat:post"]
coef_twfe <- coef(twfe)

data.frame(
  Estimateur = c("DID simple", "TWFE"),
  Coefficient = c(coef_did, coef_twfe)
)



