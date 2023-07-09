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
