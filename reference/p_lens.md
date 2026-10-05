# Lens Plot

Raw images per lens type.

## Usage

``` r
p_lens(data, fg = "#000", bg = "grey", theme = p_theme())
```

## Arguments

- data:

  a data.frame (see details).

- fg:

  a foreground color.

- bg:

  a background color.

- theme:

  an optional theme function.

## Value

a 'ggplot' object.

## Details

`data` expects the following columns:

- lens_model: a character string, the name of the lens

- n: numeric, the number of images for this lens

## See also

[`p_theme()`](https://thekangaroofactory.github.io/kraw/reference/p_theme.md)
