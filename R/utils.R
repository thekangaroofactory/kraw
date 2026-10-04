

#' Convert
#'
#' @param x a character vector.
#'
#' @returns a vector.
#' @export
#'
#' @examples
#' to_num("ffff")

to_num <- function(x){
  strtoi(x, 16L)}


#' Tag Id
#'
#' @param mapping a data.frame for the mapping.
#' @param x a character string, the name of the key.
#'
#' @returns a character string.
#' @export
#'
#' @examples
#' tag_id(mapping_exif, "ImageWidth")

tag_id <- function(mapping, x){
  mapping[mapping$key == x, ]$tag_id}
