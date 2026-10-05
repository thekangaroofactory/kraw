# Read CR2 File

Read CR2 File

## Usage

``` r
read_cr2(
  file,
  image = FALSE,
  mapping_exif = mapping_exif,
  mapping_canon = mapping_canon
)
```

## Arguments

- file:

  the file to read (including its path).

- image:

  a logical whether the images should be loaded or not (default FALSE).

- mapping_exif:

  a data.frame of the EXIF tag mapping.

- mapping_canon:

  a data.frame of the Canon tag mapping.

## Value

a list.

## Examples

``` r
if (FALSE) { # \dontrun{
read_cr2(file = "C:/download/example.CR2")
} # }
```
