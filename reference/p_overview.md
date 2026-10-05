# Overview Plot

Overview Plot

## Usage

``` r
p_overview(metadata)
```

## Arguments

- metadata:

  a data.frame (see details).

## Value

a ggplot object.

## Details

`metadata` expects an output from the
[`scan()`](https://thekangaroofactory.github.io/kraw/reference/scan.md)
function.

## See also

[`scan()`](https://thekangaroofactory.github.io/kraw/reference/scan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
viz(scan(path = "."))
} # }
```
