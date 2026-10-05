# Most Frequent

Extract most frequent combination.

## Usage

``` r
most_frequent(data)
```

## Arguments

- data:

  a data.frame (output of
  [`scan()`](https://thekangaroofactory.github.io/kraw/reference/scan.md)).

## Value

a data.frame.

## Details

The function computes the most frequent combination over: camera,
orientation, exposure_time, f_number, iso_speed, lens_model,
focal_length

## Examples

``` r
if (FALSE) { # \dontrun{
most_frequent(scan("."))
} # }
```
