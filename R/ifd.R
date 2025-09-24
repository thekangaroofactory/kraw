

#' Image File Directory
#'
#' @param x a raw vector
#' @param offset an integer to indicate where the sequence starts
#'
#' @returns a list with the IFD information
#' @export
#'
#' @examples
#' \dontrun{
#' ifd(x, offset = 924)
#' }

ifd <- function(x, offset = 1){
  
  # -- number of entries (2 bytes, ushort)
  nb_entries <- to_num(order_bytes(raw_bytes(x, offset, n = 2)))
  
  # -- sequence of entries (12 bytes each)
  # length of the sequence: 12 x nb_entries
  entries <- raw_bytes(x, offset = offset + 2, n = 12 * nb_entries)
  entries <- as.data.frame(matrix(entries, nrow = nb_entries, ncol = 12, byrow = TRUE))

    
  # -- format sequence (12 bytes):
  # tag_id = 2 bytes, ushort
  # tag_type = 2 bytes, ushort
  # tag_count = 4 bytes, ulong
  # tag_value / offset to value = 4 bytes, ulong
  entries$tag_id <- order_bytes(raw_bytes(entries, n = 2))
  entries$tag_type <- to_num(order_bytes(raw_bytes(entries, offset = 2, n = 2)))
  entries$tag_count <- to_num(order_bytes(raw_bytes(entries, offset = 4, n = 4)))
  entries$tag_value <- order_bytes(raw_bytes(entries, offset = 8, n = 4))

  
  # -- next IFD offset (4 bytes)
  offset_next_ifd <- to_num(order_bytes(raw_bytes(x, offset = offset + 2 + 12 * nb_entries, n = 4)))

  
  # -- return
  list(
    nb_entries = nb_entries,
    entries = entries[c("tag_id", "tag_type", "tag_count", "tag_value")],
    offset_next_ifd = offset_next_ifd)
  
}
