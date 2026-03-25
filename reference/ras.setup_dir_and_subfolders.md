# Setup directory & subfolders

Setup directory & subfolders

## Usage

``` r
ras.setup_dir_and_subfolders(output_dir = NULL)
```

## Arguments

- output_dir:

  A directory path

## Value

A directory path

## Examples

``` r
out_dir <- file.path(tempdir(), "aquaras_example_output")
subdirs <- ras.setup_dir_and_subfolders(out_dir)
names(subdirs)
#>  [1] "files"      "diff_limma" "diff_DEqMS" "barplots"   "foldChange"
#>  [6] "heatmaps"   "qc_VSN"     "qc_PCA"     "qc_UMAP"    "qc_tSNE"   
#> [11] "qc_DEqMS"  
```
