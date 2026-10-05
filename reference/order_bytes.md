# Order Bytes

Order Bytes

## Usage

``` r
order_bytes(x, endian = "II")
```

## Arguments

- x:

  a vector or data.frame of bytes.

- endian:

  the endianess.

## Value

the corresponding hexadecimal vector or data.frame.

## Details

When `x` is a data.frame, value will be a data.frame, each row being the
concatenation of the ordered columns.

Only little endian (i.e. "II") is supported.

## Examples

``` r
if (FALSE) { # \dontrun{
order_bytes(x, endian = "II")
} # }
```
