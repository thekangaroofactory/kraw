# Aperture Plot

Aperture Plot

## Usage

``` r
p_aperture(data, fg = "grey", bg = "grey", theme = p_theme())
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

- f_number: character, the aperture value

## Examples

``` r
if (FALSE) { # \dontrun{
p_aperture(data)
} # }
```
