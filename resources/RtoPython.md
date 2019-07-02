# Notes for converting R to Python

Assuming you are using the Pandas Library in Python

## Basic 

| Operation | R | Python 3 |
|---|---|---|
| Dimensions of dataframe | `dim(df)` | `df.shape` |
| Get number summary of dataframe |`summary(df)` | `df.describe()` |
| Data types of columns | `str(df)` | `df.info()` |
| Column selection by number | `df["col1"]` or `df$col1` | `df.iloc[:,[1]]` |
| Column selection by name | `df[c("col1", "col2")]` or `df$col1` | `df[["col1", "col2"]]` or `df.loc[:,["col1", "col2"]]` |
| Get Row Names | `rownames(df)` | `list(df.index.values)` |
| Get Column Names | `colnames(df)` | `list(df.columns.values)` |

## Files

| Operation | R | Python 3 |
|---|---|---|
| Read CSV | `read.csv(file)` | `pd.read_csv(file)` |
| Write CSV | `write.csv(df, file)` | `df.to_csv(file)` |

