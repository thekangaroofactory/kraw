# Orientation Plot

Distribution over landscape and portrait.

## Usage

``` r
p_orientation(nl, np, fg = "black", bg = "grey", theme = p_theme())
```

## Arguments

- nl:

  a numeric, the number of landscape.

- np:

  a numeric, the number of portraits.

- fg:

  a foreground color.

- bg:

  a background color.

- theme:

  an optional theme function.

## Value

a 'ggplot' object.

## See also

[`p_theme()`](https://thekangaroofactory.github.io/kraw/reference/p_theme.md)

## Examples

``` r
p_orientation(nl = 42, np = 112)
```
