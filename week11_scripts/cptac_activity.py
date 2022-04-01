import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import ../analysis_data/cptac/cptac_protein.csv
import ../analysis_data/cptac/cptac_rna.csv

protein_df = cptac_protein.csv
rna_df = cptac_rna.csv

def intersectDataFrames(
    np.intersect1d(protein_df, rna_df))

def creatCorrelaitonMatrix(
    gene = "KRAS"
    corr, pval = stats.spearmanr(rna_df[gene], protein_df[gene],␣
    , →nan_policy = "omit"))

def createCorrelationHeatmap(
    ncomparisons = 20
    gene_names = rna_df.columns[0:ncomparisons]
    corr_df = pd.DataFrame(np.ndarray(shape=(ncomparisons, ncomparisons), dtype=np.
                                      ,→float16)))

    index = gene_names,
            columns = gene_names)

    13

    for g1 in gene_names:
        for g2 in gene_names:
            corr, p = stats.spearmanr(
                rna_df[g1],
                protein_df[g2],
                nan_policy="omit"
            )
    corr_df.loc[g1, g2] = corr

    sns.heatmap(
        corr_df,
        cmap='mako'
    )
    plt.show()

def main():

if __name__ == "__main__":
    main()