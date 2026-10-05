# kraw

Read and extract metadata information from CR2 raw camera images.

## Installation

The package can be installed from R-universe:

``` r
# Install 'kraw' in R:
install.packages('kraw', repos = c('https://thekangaroofactory.r-universe.dev', 'https://cloud.r-project.org'))
```

## Get started

Generate the metadata from a path containing .cr2 files:

``` r
library(kraw)

# scan files
metadata <- scan(path = ".")
```

Once the metadata table is ready, build the print:

``` r
# plot
p_overview(metadata)
```

## References

This package implements the CR2 schema as described in this document:\
<http://lclevy.free.fr/cr2/>
