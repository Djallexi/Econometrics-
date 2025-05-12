import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns

# Génération de données aléatoires
np.random.seed(42)
X = np.random.normal(0, 1, 100)
Y = 2 + 3 * X + np.random.normal(0, 1, 100)

# Création d’un DataFrame
df = pd.DataFrame({'X': X, 'Y': Y})

# Statistiques descriptives
print("\nStatistiques descriptives de base :")
print(df.describe())

# Corrélation
print("\nMatrice de corrélation :")
print(df.corr())

# Régression linéaire
X_ = sm.add_constant(df['X'])  # Ajout de l’ordonnée à l’origine
model = sm.OLS(df['Y'], X_).fit()
print("\nRésultats de la régression :")
print(model.summary())

# Visualisation : Nuage de points + droite de régression
sns.set(style="whitegrid")
plt.figure(figsize=(8, 5))
sns.regplot(x='X', y='Y', data=df, ci=None, line_kws={'color': 'red'})
plt.title("Régression linéaire : Y ~ X")
plt.xlabel("X")
plt.ylabel("Y")
plt.show()
