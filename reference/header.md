# CR2 Headers

Extract the TIFF and Canon headers.

## Usage

``` r
header(x)
```

## Arguments

- x:

  a raw vector.

## Value

a list.

## Details

The function returns a list with the following elements:

- endianness: order in which the bites are written (expecting little
  endian / II)

- magic_number: TIFF signature (expecting 42)

- offset_first_ifd: offset to first ifd

- cr_marker: raw marker (expecting 'CR+2')

- cr_version: raw marker version

- offset_ifd_raw: offset to first raw ifd

## Examples

``` r
if (FALSE) { # \dontrun{
header(x)
} # }
```
