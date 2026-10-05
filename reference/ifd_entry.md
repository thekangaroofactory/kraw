# IFD Entry Value or Offset

IFD Entry Value or Offset

## Usage

``` r
ifd_entry(ifd, id, raw_vector = NULL, verbose = FALSE)
```

## Arguments

- ifd:

  an IFD list.

- id:

  the id of the entry.

- raw_vector:

  the raw vector to extract value when an offset is involved.

- verbose:

  whether to turn ON/OFF console log.

## Value

a numeric value or a vector with the offset.

## Details

`ifd ` expects a list as returned by the
[`ifd()`](https://thekangaroofactory.github.io/kraw/reference/ifd.md)
function.

## See also

[`ifd()`](https://thekangaroofactory.github.io/kraw/reference/ifd.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ifd_entry(ifd, id = "0100")
} # }
```
