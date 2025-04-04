library(tidyverse)
library(magick)
library(glue)
library(colorspace)

# Load `header` list with needed data
header <- readRDS("R/portraits/channel_islands/header.rds")
colors <- header$colors
swatchplot(colors)

text_color <- colors[4]

img <- image_read(header$outfile)
image_info(img)

# s <- darken("#9a9397", .1)
# 
# shadow <- "#9a9397"
# inset <- image_read("images/channel_islands/tam_inset.png")


img |> 
  image_crop(geometry = "6000x4500+0+500", gravity = "north") |> 
  image_annotate(text = "CHANNEL ISLANDS", 
                 gravity = "north",
                 location = "+0+600", font = "Poller One",
                 color = text_color, kerning = 50,
                 size = 350, weight = 700) |> 
  image_annotate(text = "NATIONAL PARK",
                 gravity = "north",
                 location = "+0+2700", font = "Amarante",
                 color = colors[8], kerning = 110,
                 weight = 700,
                 size = 450) |>
  image_annotate(text = glue("Graphic by Spencer Schien (@MrPecners) | ",
                             "Data from the National Park Service"),
                 gravity = "north",
                 location = "+0+3200", font = "Amarante",
                 color = colors[8],
                 kerning = 22,
                 size = 80) |>
  # image_composite(image_scale(inset, geometry = "75%x"),
  #                 gravity = "southwest",
  #                 offset = "+500+750") |> 
  image_write("images/channel_islands/titled_ci_pop.png")

image_read("images/channel_islands/titled_ci_pop.png") |> 
  image_scale(geometry = "70%x") |> 
  image_write("tracked_graphics/titled_ci_pop_small.png")
