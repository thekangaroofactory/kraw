# Image File Directory

Image File Directory

## Usage

``` r
ifd(x, offset = 1)
```

## Arguments

- x:

  a raw vector.

- offset:

  an integer to indicate where the sequence starts.

## Value

a list with the IFD information.

## Details

The output list contains:

- nb_entries: the number of entries

- entries: the entries

- offset_next_ifd: offset to the next ifd.

## Examples

``` r
if (FALSE) { # \dontrun{
ifd(x, offset = 924)
} # }
```
