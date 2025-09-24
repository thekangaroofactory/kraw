

#' Order Bytes
#'
#' @param x a vector or data.frame of bytes
#' @param endian the endianess
#'
#' @returns the corresponding hexadecimal vector or data.frame
#' @export
#'
#' @details
#' When x is a data.frame, value will be a data.frame, each row being
#' the concatenation of the ordered columns
#' 
#' @examples
#' \dontrun{
#' order_bytes(x, endian = "II")
#' }

order_bytes <- function(x, endian = "II"){
  
  stopifnot("Only little endian is supported!" = endian == "II")
  
  # -- case data.frame
  if(is.data.frame(x))
    return(do.call(paste, c(x[ncol(x):1], sep = "")))
  
  # -- case vector / default
  paste(rev(x), collapse = "")
  
}
