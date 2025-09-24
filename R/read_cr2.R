

#' Read CR2 File
#'
#' @param file the file to read (including path)
#' @param image a logical whether the images should be loaded or not (default FALSE)
#' @param mapping_exif a data.frame of the EXIF tag mapping
#' @param mapping_canon a data.frame of the Canon tag mapping
#'
#' @returns a list of metadata
#' @export
#'
#' @examples
#' \dontrun{
#' read_cr2(file = "C:/download/example.CR2")
#' }

read_cr2 <- function(file, image = FALSE, mapping_exif = NULL, mapping_canon = NULL){

  # -- read binary file
  # n = 16 (= one row) * nb row to read
  raw_vector <- readBin(file, what = "raw", n = 2120000)


  # ----------------------------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------------------------

  # -- extract header
  header <- header(raw_vector)


  # ----------------------------------------------------------------------------
  # IFD_0
  # ----------------------------------------------------------------------------
  # offset in header

  # -- extract IFD
  ifd_0 <- ifd(raw_vector, header$offset_first_ifd)

  # -- available tags
  # mapping_exif[match(ifd_0$entries$tag_id, mapping_exif$tag_id), ]$key

  # -- extract values from IFD
  image_width <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "ImageWidth"))
  image_height <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "ImageLength"))
  orientation <-  ifd_entry(ifd_0, id = tag_id(mapping_exif, "Orientation"))
  make <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "Make"), raw_vector)
  model <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "Model"), raw_vector)
  date_time <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "DateTime"), raw_vector)

  # -- extract image info
  image_0_offset <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "StripOffsets"))
  image_0_length <- ifd_entry(ifd_0, id = tag_id(mapping_exif, "StripByteCounts"))

  # -- extract offset
  offset_exif <- ifd_entry(ifd_0, id = "8769")


  # ----------------------------------------------------------------------------
  # EXIF SUB-IFD
  # ----------------------------------------------------------------------------
  # offset in IFD_0

  # -- extract IFD
  ifd_exif <- ifd(raw_vector, offset_exif)

  # -- available tags
  # mapping_exif[match(ifd_exif$entries$tag_id, mapping_exif$tag_id), ]$key


  # -- extract values from IFD

  # -- lens model
  lens_model <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "LensModel"), raw_vector)

  # -- exposure time
  exposure_time <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "ExposureTime"), raw_vector)
  exposure_time <- paste(exposure_time$numerator, exposure_time$denominator, sep = "/")

  # -- f number
  f_number <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "FNumber"), raw_vector)
  f_number <- paste("f", f_number$numerator / f_number$denominator, sep = "/")

  # -- focal length
  focal_length <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "FocalLength"), raw_vector)
  focal_length <- focal_length$numerator / focal_length$denominator

  # -- ISO
  iso_speed_ratings <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "ISOSpeedRatings"), raw_vector)

  # -- misc
  exposure_program <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "ExposureProgram"), raw_vector)
  metering_mode <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "MeteringMode"), raw_vector)
  flash <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "Flash"), raw_vector)

  # -- not sure about those ones (useful?)
  exposure_mode <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "ExposureMode"), raw_vector)
  white_balance <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "WhiteBalance"), raw_vector)
  scene_capture_type <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "SceneCaptureType"), raw_vector)
  body_serial_number <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "BodySerialNumber"), raw_vector)
  lens_serial_number <- ifd_entry(ifd_exif, id = tag_id(mapping_exif, "LensSerialNumber"), raw_vector)


  # -- offset
  offset_makernote <- ifd_entry(ifd_exif, id = "927c")


  # ----------------------------------------------------------------------------
  # MAKERNOTE SUB-IFD
  # ----------------------------------------------------------------------------
  # offset in EXIF

  # -- extract IFD
  ifd_makernote <- ifd(raw_vector, offset_makernote['offset'])

  # -- available tags
  # mapping_canon[match(ifd_makernote$entries$tag_id, mapping_canon$tag_id), ]$key
  # note: many of them are already in exif, others are offsets to more data


  # ----------------------------------------------------------------------------
  # IFD_1 (thumbnail)
  # ----------------------------------------------------------------------------
  # offset in ifd_0

  # -- extract IFD
  ifd_1 <- ifd(raw_vector, ifd_0$offset_next_ifd)

  # -- available tags
  # mapping_exif[match(ifd_1$entries$tag_id, mapping_exif$tag_id), ]$key

  # -- extract image info
  image_1_offset <- ifd_entry(ifd_1, id = tag_id(mapping_exif, "JPEGInterchangeFormat"), raw_vector)
  image_1_length <- ifd_entry(ifd_1, id = tag_id(mapping_exif, "JPEGInterchangeFormatLength"), raw_vector)


  # ----------------------------------------------------------------------------
  # IFD_2
  # ----------------------------------------------------------------------------
  # offset in ifd_1

  # -- extract IFD
  ifd_2 <- ifd(raw_vector, ifd_1$offset_next_ifd)

  # -- available tags
  # mapping_exif[match(ifd_2$entries$tag_id, mapping_exif$tag_id), ]$key

  # -- extract image info
  image_2_width <- ifd_entry(ifd_2, id = tag_id(mapping_exif, "ImageWidth"), raw_vector)
  image_2_height <- ifd_entry(ifd_2, id = tag_id(mapping_exif, "ImageLength"), raw_vector)
  image_2_offset <- ifd_entry(ifd_2, id = tag_id(mapping_exif, "StripOffsets"), raw_vector)
  image_2_length <- ifd_entry(ifd_2, id = tag_id(mapping_exif, "StripByteCounts"), raw_vector)


  # ----------------------------------------------------------------------------
  # IFD_3
  # ----------------------------------------------------------------------------
  # offset in ifd_2 == header$offset_ifd_raw

  # -- extract IFD
  ifd_3 <- ifd(raw_vector, header$offset_ifd_raw)

  # -- available tags
  # mapping_exif[match(ifd_3$entries$tag_id, mapping_exif$tag_id), ]$key

  # -- extract image info
  image_3_width <- ifd_entry(ifd_3, id = tag_id(mapping_exif, "ImageWidth"), raw_vector)
  image_3_height <- ifd_entry(ifd_3, id = tag_id(mapping_exif, "ImageLength"), raw_vector)
  image_3_offset <- ifd_entry(ifd_3, id = tag_id(mapping_exif, "StripOffsets"), raw_vector)
  image_3_length <- ifd_entry(ifd_3, id = tag_id(mapping_exif, "StripByteCounts"), raw_vector)


  # -- chech parameter
  if(image){

    # --------------------------------------------------------------------------
    # IMAGE_1
    # --------------------------------------------------------------------------
    # offset & length in IFD_1

    # -- extract image
    image_1 <- raw_bytes(raw_vector, offset = image_1_offset, n = image_1_length)



    # --------------------------------------------------------------------------
    # IMAGE_0
    # --------------------------------------------------------------------------
    # offset & length in IFD_0

    # -- extract image
    image_0 <- raw_bytes(raw_vector, offset = image_0_offset, n = image_0_length)


    # --------------------------------------------------------------------------
    # IMAGE_2
    # --------------------------------------------------------------------------
    # offset & length in IFD_2

    # -- extract image
    image_2 <- raw_bytes(raw_vector, offset = image_2_offset, n = image_2_length)

    # image_matrix <- matrix(image_2, nrow = image_2_width, ncol = image_2_height, byrow = TRUE)
    # plot(as.raster(image_matrix))


    # --------------------------------------------------------------------------
    # IMAGE_3
    # --------------------------------------------------------------------------
    # offset & length in IFD_3

    # -- extract image
    image_3 <- raw_bytes(raw_vector, offset = image_3_offset, n = image_3_length)

  }


  # ----------------------------------------------------------------------------
  # Return value
  # ----------------------------------------------------------------------------

  list(
    path = dirname(file),
    filename = basename(file),
    filetype = paste0(header$cr_marker, header$cr_version),
    make = make,
    camera = model,
    date_time = date_time,
    image_height = image_height,
    image_width = image_width,
    orientation = orientation,
    exposure_time = exposure_time,
    f_number = f_number,
    iso_speed = iso_speed_ratings,
    lens_model = lens_model,
    focal_length = focal_length,
    exposure_program = exposure_program,
    metering_mode = metering_mode,
    exposure_mode = exposure_mode,
    white_balance = white_balance,
    scene_capture_type = scene_capture_type,
    flash = flash,
    body_serial_number = body_serial_number,
    lens_serial_number = lens_serial_number)

}
