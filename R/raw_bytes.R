

#' Sequence of Bytes
#'
#' @param x a vector of raw bytes
#' @param offset an integer, to indicate the lower limit of the sequence
#' @param n an integer, to indicate the length of the sequence
#'
#' @returns a vector of raw bytes
#' @export
#' 
#' @details
#' Note that offset = 0 (default) is the index of the first element of the list
#' 
#'
#' @examples
#' \dontrun{
#' # get a sequence of 4 bytes at offset 294
#' raw_bytes(x, offset = 294, n = 4)
#' }

raw_bytes <- function(x, offset = 0, n = 1){
  
  # -- check x length
  if(length(x) < offset + n)
    stop("Raw vector is too short!")
  
  # -- return
  x[(offset + 1):(offset + n)]
  
}
