# Focal Length Plot

Focal Length Plot

## Usage

``` r
p_focal(data, fg = "grey", bg = "grey", theme = p_theme())
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

- lens_model: character, the name of the lens

- focal_length: numeric, the focal length

- n: the number of images.

## Examples

``` r
if (FALSE) { # \dontrun{
p_focal(data = metadata |>
dplyr::group_by(lens_model, focal_length) |>
dplyr::summarise(n = dplyr::n()))
} # }
```
