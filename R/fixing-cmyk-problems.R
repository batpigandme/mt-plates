# dealing with CMYK issues
library(tidyverse)
library(magick)

# read in trimmed images --------------------------------------------------
plate_names <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  basename()

plates <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  map(image_read) |>
  set_names(plate_names)

plate_info <- plates |>
  map(image_info) |>
  list_rbind()

plate_name_df <- tibble::tibble("name" = plate_names)

plate_info <- plate_name_df |>
  bind_cols(plate_info) |>
  mutate(path = glue::glue("imgs/trimmed/{name}"))

cmyk_plate_info <- plate_info |>
  filter(colorspace == "CMYK")

cmyk_plate_files <- cmyk_plate_info$path

cmyk_plates <- cmyk_plate_files |>
  map(image_read) |>
  set_names(cmyk_plate_info$name)

image_convert(cmyk_plates[[1]], colorspace = "sRGB")

# trim files and create new -----------------------------------------------
walk(
  cmyk_plate_files,
  \(cmyk_plate_files)
  image_read(cmyk_plate_files) |>
  image_convert(colorspace = "sRGB") |>
  image_write(path = cmyk_plate_files)
)


# check to see if all plates now sRGB -------------------------------------
plate_names <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  basename()

plates <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  map(image_read) |>
  set_names(plate_names)

plate_info <- plates |>
  map(image_info) |>
  list_rbind()

plate_name_df <- tibble::tibble("name" = plate_names)

plate_info <- plate_name_df |>
  bind_cols(plate_info) |>
  mutate(path = glue::glue("imgs/trimmed/{name}"))

cmyk_plate_info <- plate_info |>
  filter(colorspace == "CMYK")


# redo collage ------------------------------------------------------------

# Desired plate resolution in pixels
plate_width <- 400

# Scale all plate to the desired pixel width
plates <- plates |>
  map(image_scale, plate_width)

# override plate height for 2:1 aspect ratio
plate_height <- 200

# Coerce plate dimensions
plates <- plates %>%
  map(image_resize, paste0(plate_width, "x", plate_height, "!"))

# Sort plates by colour
plate_col <- plates %>%
  map(image_resize, "1x1!") %>%
  map(image_data) %>%
  map(~paste0("#", paste0(.[,,1], collapse=""))) %>%
  map(colorspace::hex2RGB) %>%
  map(as, "HSV") %>%
  map_dbl(~.@coords[,1]) %>%
  sort(index.return = TRUE) %>%
  .$ix

plates <- plates[plate_col]

plate_row_size <- 9
# Calculate row sizes
plate_col_size <- ceiling(length(plates)/plate_row_size)
row_lens <- rep(c(plate_row_size), length.out=plate_col_size)
row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(plates) - sum(row_lens))

plate_rows <- map2(row_lens, cumsum(row_lens),
                   ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
  map(~ plates[.x] %>%
        invoke(c, .) %>%
        image_append)

image_append(plate_rows, stack = TRUE)

# Add plates to canvas
canvas <- image_blank(plate_row_size*plate_width,
                      plate_height*plate_col_size,
                      "white")

mt_plate_collage <- reduce2(plate_rows, seq_along(plate_rows),
                            ~ image_composite(
                              ..1, ..2,
                              offset = paste0("+", 0,
                                              "+", round((..3-1)*plate_height)) # no extra height for first row
                            ),
                            .init = canvas)

fs::dir_create("imgs/collage")
image_write(mt_plate_collage, "imgs/collage/mt_plate_collage.jpg")
