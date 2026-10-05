# ISO Plot

ISO Plot

## Usage

``` r
p_iso(data, bg = "grey", theme = p_theme())
```

## Arguments

- data:

  a data.frame (see details).

- bg:

  a background color.

- theme:

  an optional theme function.

## Value

a 'ggplot' object.

## Details

`data` expects the following columns:

- lens_model: character, the name of the lens

- iso_speed: numeric, the ISO speed

- n: the number of images.

## Examples

``` r
if (FALSE) { # \dontrun{
p_iso(data.frame(lens_model = "foo", iso_speed = 100, n = 3))
} # }
```
