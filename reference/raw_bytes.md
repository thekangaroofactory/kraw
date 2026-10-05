# Sequence of Bytes

Sequence of Bytes

## Usage

``` r
raw_bytes(x, offset = 0, n = 1)
```

## Arguments

- x:

  a vector of raw bytes.

- offset:

  an integer, to indicate the lower limit of the sequence.

- n:

  an integer, to indicate the length of the sequence.

## Value

a vector of raw bytes.

## Details

Note that offset = 0 (default) is the index of the first element of the
list.

## Examples

``` r
if (FALSE) { # \dontrun{
# get a sequence of 4 bytes at offset 294
raw_bytes(x, offset = 294, n = 4)
} # }
```
