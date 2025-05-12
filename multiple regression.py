import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.stats.diagnostic import het_breuschpagan
from scipy import stats

# Générer des données
np.random.seed(42)
n = 200
X1 = np.random.normal(0, 1, n)
X2 = np.random.normal(5, 2, n)
Y = 1.5 + 2.0*X1 + 0.5*X2 + np.random.normal(0, 1, n)

# DataFrame
df = pd.DataFrame({'X1': X1, 'X2': X2, 'Y': Y})

# Statistiques descriptives
print("Statistiques descriptives :")
print(df.describe())

# Régression multiple
X = sm.add_constant(df[['X1', 'X2']])
model = sm.OLS(df['Y'], X).fit()
print("\nRésumé du modèle :")
print(model.summary())

# Résidus
residuals = model.resid

# --- Test de normalité des résidus (Shapiro-Wilk)
print("\nTest de normalité des résidus (Shapiro-Wilk) :")
shapiro_test = stats.shapiro(residuals)
print(f"Statistique W = {shapiro_test.statistic:.4f}, p-value = {shapiro_test.pvalue:.4f}")
if shapiro_test.pvalue > 0.05:
    print("✔️ Les résidus suivent une loi normale (à 5%)")
else:
    print("❌ Les résidus ne suivent pas une loi normale")

# --- Test d'hétéroscédasticité (Breusch-Pagan)
print("\nTest de Breusch-Pagan pour l'hétéroscédasticité :")
bp_test = het_breuschpagan(residuals, model.model.exog)
bp_stat, bp_pval = bp_test[0], bp_test[1]
print(f"Statistique = {bp_stat:.4f}, p-value = {bp_pval:.4f}")
if bp_pval > 0.05:
    print("✔️ Homoscédasticité acceptée (pas de preuve d'hétéroscédasticité)")
else:
    print("❌ Hétéroscédasticité détectée")

# --- Visualisation des résidus
plt.figure(figsize=(10, 5))
sns.histplot(residuals, kde=True)
plt.title("Distribution des résidus")
plt.show()

plt.figure(figsize=(8, 5))
sns.scatterplot(x=model.fittedvalues, y=residuals)
plt.axhline(0, color='red', linestyle='--')
plt.title("Résidus vs valeurs ajustées")
plt.xlabel("Valeurs ajustées")
plt.ylabel("Résidus")
plt.show()
