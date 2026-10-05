

#' CR2 Headers
#'
#' @description
#' Extract the TIFF and Canon headers.
#'
#' @param x a raw vector.
#'
#' @details
#' The function returns a list with the following elements:
#' -  endianness: order in which the bites are written (expecting little endian / II)
#' - magic_number: TIFF signature (expecting 42)
#' - offset_first_ifd: offset to first ifd
#' - cr_marker: raw marker (expecting 'CR+2')
#' - cr_version: raw marker version
#' - offset_ifd_raw: offset to first raw ifd
#'
#' @returns a list.
#' @export
#'
#' @examples
#' \dontrun{
#' header(x)
#' }

header <- function(x){

  # ////////////////////////////////////////////////////////////////////////////
  # TIFF

  # -- First 8 bytes
  raw_tiff <- raw_bytes(x, n = 8)

  # -- Endianess
  endianness <- rawToChar(raw_bytes(raw_tiff, n = 2))
  stopifnot("CR2 file should be written with little endian" = endianness == "II")

  # -- TIFF magic number (expecting 42)
  magic_number <- to_num(order_bytes(raw_bytes(raw_tiff, offset = 2, n = 2)))

  # -- Offset to first IFD
  offset_first_ifd <- to_num(order_bytes(raw_bytes(raw_tiff, offset = 4, n = 4)))


  # ////////////////////////////////////////////////////////////////////////////
  # CR2

  # -- Second 8 bytes
  raw_cr2 <- raw_bytes(x, offset = 8, n = 8)

  # -- Raw marker (expecting CR+2)
  cr_marker <- rawToChar(raw_bytes(raw_cr2, n = 2))
  cr_version <- to_num(order_bytes(raw_bytes(raw_cr2, offset = 2, n = 2)))

  # -- Offset to raw IFD
  offset_ifd_raw <- to_num(order_bytes(raw_bytes(raw_cr2, offset = 4, n = 4)))


  # ////////////////////////////////////////////////////////////////////////////
  # Return

  list(
    endianness = endianness,
    magic_number = magic_number,
    offset_first_ifd = offset_first_ifd,
    cr_marker = cr_marker,
    cr_version = cr_version,
    offset_ifd_raw = offset_ifd_raw)

}
