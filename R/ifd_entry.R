

ifd_entry <- function(ifd, id, raw_vector = NULL){
  
  entries <- ifd$entries
  
  # -- check tag id exists
  if(id %in% entries$tag_id){
    
    # -- get entry
    entry <- entries[entries$tag_id == id, ]
    
    # -- entry types -----------------------------------------------------------
    
    # -- 2: string, ASCII 0 terminated -----------------------------------------
    if(entry$tag_type == 2){
      
      cat("Type = 2 (string), read offset sequence \n")
      
      x <- raw_bytes(raw_vector,
                     offset = to_num(entry$tag_value), 
                     n = entry$tag_count)
      
      return(rawToChar(x))}
    
    
    # -- 3: ushort, unsigned 16 bits -------------------------------------------
    # out of 4 bytes available, should contain 0 for the 2 other bytes
    if(entry$tag_type == 3){
      
      # -- check count
      # 1 & 2 fit in tag_value
      x <- if(entry$tag_count <= 2)
        entry$tag_value
      else {
        cat("Type = 2 (ushort), read offset sequence \n")
        order_bytes(
          raw_bytes(raw_vector, 
                    offset = to_num(entry$tag_value), 
                    n = entry$tag_count))}
        
      return(to_num(x))} 
    
    # -- 4: ulong, unsigned 32bits ---------------------------------------------
    # 32bits = 4 bytes
    if(entry$tag_type == 4){
      
      stopifnot("Count > 1 not supported yet!" = entry$tag_count == 1)
      
      x <- if(entry$tag_count == 1)
        entry$tag_value
      else {
        
        stop("This code has not been checked")
        cat("Type = 4 (ulong), read offset sequence \n")
        order_bytes(
          raw_bytes(raw_vector, 
                    offset = to_num(entry$tag_value),
                    n = entry$tag_count * 4))}
      
      return(to_num(x))}
    
    
    # -- 5: urational, numerator / denominator ulongs --------------------------
    # ulong + ulong = 4 + 4
    if(entry$tag_type == 5){
      
      stopifnot("Count > 1 not supported yet!" = entry$tag_count == 1)
      cat("Type = 5 (urational), read offset sequences \n")
      
      # -- numerator
      num <- order_bytes(raw_bytes(raw_vector, 
                                   offset = to_num(entry$tag_value),
                                   n = 4))
      
      # -- denominator
      den <- order_bytes(raw_bytes(raw_vector, 
                                   offset = to_num(entry$tag_value) + 4,
                                   n = 4))
    
      return(list(numerator = to_num(num), denominator = to_num(den)))}
    
    # -- 7: ubyte, sequence ----------------------------------------------------
    # return offset + tag_count
    return(c(offset = to_num(entry$tag_value), n = entry$tag_count))
    
    
    # -- type 10: rational, signed 2 longs (= 2 sequences of 4)
    # offset <- ifd_entry(exif, id = "9201")
    # v1 <- raw_bytes(raw_vector, offset, n = 4)
    # hex_to_num(v1)
    # v2 <- raw_bytes(raw_vector, offset + 4, n = 4)
    # hex_to_num(v2)
    # >> so what?
    
    
    # -- default
    warning("Entry type is not implemented, returning NA!")
    NA
    
  } else NA
  
}
