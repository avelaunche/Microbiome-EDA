import pandas as pd

microbiomedata = pd.read_csv("microbiomedata.csv", index_col=0)
sam_table = pd.read_csv("metadata.csv", index_col=0)

print(microbiomedata.shape)
print(sam_table.shape)

print(microbiomedata.head())
print(sam_table.head())
