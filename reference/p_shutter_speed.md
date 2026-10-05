# Shutter Speed Plot

Shutter Speed Plot

## Usage

``` r
p_shutter_speed(data, fg = "grey", bg = "grey", theme = p_theme())
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

- exposure_time: character, the aperture value

- n: the number of images.

## Examples

``` r
if (FALSE) { # \dontrun{
p_shutter_speed(data)
} # }
```
