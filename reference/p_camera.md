# Camera Plot

Camera Plot

## Usage

``` r
p_camera(data, fg = NA, bg = "grey", theme = p_theme())
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

`data` expects a data.frame with the following columns:

- camera: character, the camera name

- n: numeric, the number of images for the camera

## See also

[`p_theme()`](https://thekangaroofactory.github.io/kraw/reference/p_theme.md)

## Examples

``` r
p_camera(data.frame(camera = "Canon EOS 70D", n = 10))
```
