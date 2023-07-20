library(tidyverse)
library(magick)

# create dir and names for trimmed images ---------------------------------
fs::dir_create("imgs/trimmed")

# load plate images -------------------------------------------------------
plate_files <- fs::dir_ls("imgs", glob = "*.jpg")
n_plates <- length(plate_files)

new_plate_files <- map(plate_files, .f = \(file) glue::glue("imgs/trimmed/{basename(file)}") |> as.vector())


# trim files and create new -----------------------------------------------
walk2(
  plate_files,
  new_plate_files,
  \(plate_files, new_plate_files)
  image_read(plate_files) |>
    image_trim() |>
    image_write(path = new_plate_files)
  )

# read in trimmed images --------------------------------------------------
plate_names <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  basename()

plates <- fs::dir_ls("imgs/trimmed", glob = "*.jpg") |>
  map(image_read) |>
  set_names(plate_names)

# adapting from Mitch O'Hara-Wild's Arranging Hex Stickers in R
# https://www.mitchelloharawild.com/blog/hexwall/

# Desired sticker resolution in pixels
plate_width <- 400

# Scale all stickers to the desired pixel width
plates <- plates |>
  map(image_scale, plate_width)

# Identify low resolution stickers
plates |>
  map_lgl(~ with(image_info(.x),
                 width < (plate_width-1)/2 && format != "svg")
  )

# Identify CMYK plates
plates |>
  map_lgl(~ with(image_info(.x),
                 colorspace == "CMYK")) -> colorspace_plates

# Identify incorrect shapes / proportions (tolerance of +-2 height)
plates |>
  map_lgl(~ with(image_info(.x),
                 height < (median(height)-2) | height > (median(height) + 2))
  )

# Extract correct sticker height (this could also be calculated directly from width)
plate_height <- plates %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median

# override plate height for 2:1 aspect ratio
plate_height <- 200

# Coerce sticker dimensions
plates <- plates %>%
  map(image_resize, paste0(plate_width, "x", plate_height, "!"))

plates[["salish_kootenai_college.jpg"]]

# Sort stickers by colour
# src: https://github.com/mitchelloharawild/hexwall/blob/f3e67a968d972bf6bc86923f0985ded464b7b8cd/hexwall.R#L82-L91
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

plates[[2]]

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

plate_rows[[1]]
plate_rows[[2]]
plate_rows[[3]]

image_append(plate_rows, stack = TRUE)

# Add stickers to canvas
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
