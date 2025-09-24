
# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# -- hexadecimal to integer
to_num <- function(x) strtoi(x, 16L)

# -- 
tag_id <- function(mapping, x) mapping[mapping$key == x, ]$tag_id
