import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import spearmanr
from statsmodels.stats.multitest import multipletests
from sklearn.cross_decomposition import PLSRegression

microbiomedata_t = pd.read_csv("microbiomedata.csv", index_col=0)
sam_table = pd.read_csv("metadata.csv", index_col=0)

print(microbiomedata_t.shape)
print(sam_table.shape)

print(microbiomedata_t.head())
print(sam_table.head())


# --- load microbiome data (already as pandas DataFrame) ---

print(microbiomedata_t.info())
print(microbiomedata_t.head())

# --- visualize Clostridioides.difficile distribution ---
sns.histplot(microbiomedata_t["Clostridioides.difficile"], bins=30, kde=False)
plt.title("Distribution of C. difficile counts")
plt.show()

cdiff = microbiomedata_t["Clostridioides.difficile"]

# --- compute Spearman correlations with each species ---
corrs, pvalues, species = [], [], []

X = microbiomedata_t.drop(columns=["Clostridioides.difficile"])

for col in X.columns:
    rho, pval = spearmanr(X[col].astype(float), cdiff.astype(float))
    species.append(col)
    corrs.append(rho)
    pvalues.append(pval)

# --- adjust p-values (FDR) ---
p_adj = multipletests(pvalues, method='fdr_bh')[1]

# --- aggregate correlation data ---
cor_df = pd.DataFrame({
    "species": species,
    "correlation": corrs,
    "p_value": p_adj
})

# --- order by p-value ---
cor_df = cor_df.sort_values("p_value")
print(cor_df.head())

# --- filter significant correlations ---
cor_relevant = cor_df.query("p_value < 0.05").sort_values("p_value")
corr_select_species = cor_relevant.head(30)
print(corr_select_species)

# --- bar plot of correlations ---
plt.figure(figsize=(8, 10))
sns.barplot(
    data=corr_select_species,
    x="correlation",
    y=pd.Categorical(corr_select_species["species"], 
                     categories=corr_select_species["species"], 
                     ordered=True),
    palette="viridis"
)
plt.title("Spearman Correlation: C. difficile vs Gut Microbiota")
plt.xlabel("Correlation")
plt.ylabel("Species")
plt.show()

# --- sPLS equivalent using PLSRegression (simplified) ---
# (mixOmics::spls implements sparsity via feature selection; 
#  in Python, you can use scikit-learn's PLS regression or sparse PLS packages)
pls = PLSRegression(n_components=3)
pls.fit(X, cdiff)

# variable importance = sum of absolute loadings per feature
vip_scores = np.sum(np.abs(pls.x_weights_), axis=1)
vip_df = pd.DataFrame({
    "species": X.columns,
    "vip_score_total": vip_scores
})
vip_df = vip_df.sort_values("vip_score_total", ascending=False)
spls_select_species = vip_df

# --- plot variable importance ---
plt.figure(figsize=(8, 10))
sns.barplot(
    data=spls_select_species.head(30),
    x="vip_score_total",
    y=pd.Categorical(
        spls_select_species.head(30)["species"],
        categories=spls_select_species.head(30)["species"],
        ordered=True
    ),
    palette="magma"
)
plt.title("sPLS (PLSRegression) Variable Importance")
plt.xlabel("Variable Weight (VIP total)")
plt.ylabel("Species")
plt.show()