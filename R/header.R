

#' CR2 Header
#'
#' @param x a raw vector
#'
#' @returns a list of values (TIFF & Canon headers)
#' @export
#'
#' @examples
#' \dontrun{
#' header(x)
#' }

header <- function(x){
  
  # -- TIFF HEADER -------------------------------------------------------------
  
  # -- First 8 bytes
  raw_tiff <- raw_bytes(x, n = 8)
  
  # -- Endianess
  endianness <- rawToChar(raw_bytes(raw_tiff, n = 2))
  stopifnot("CR2 file should be written with little endian" = endianness == "II")
  
  # -- TIFF magic number (expecting 42)
  magic_number <- to_num(order_bytes(raw_bytes(raw_tiff, offset = 2, n = 2)))
  
  # -- Offset to first IFD
  offset_first_ifd <- to_num(order_bytes(raw_bytes(raw_tiff, offset = 4, n = 4)))
  
  
  # -- CR2 HEADER --------------------------------------------------------------
  
  # -- Second 8 bytes
  raw_cr2 <- raw_bytes(x, offset = 8, n = 8)
  
  # -- Raw marker (expecting CR+2)
  cr_marker <- rawToChar(raw_bytes(raw_cr2, n = 2))
  cr_version <- to_num(order_bytes(raw_bytes(raw_cr2, offset = 2, n = 2)))
  
  # -- Offset to raw IFD
  offset_ifd_raw <- to_num(order_bytes(raw_bytes(raw_cr2, offset = 4, n = 4)))
  
  
  # -- Return value ------------------------------------------------------------
  list(
    endianness = endianness,
    magic_number = magic_number,
    offset_first_ifd = offset_first_ifd,
    cr_marker = cr_marker,
    cr_version = cr_version,
    offset_ifd_raw = offset_ifd_raw)
  
}
