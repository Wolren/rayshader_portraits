library(sf)
library(tidyverse)
library(elevatr)
library(rayshader)
library(glue)
library(colorspace)
library(NatParksPalettes)
library(MetBrewer)
library(scico)
library(osmdata)

###################################
# Set up polygon for clipping DEM #
###################################

# Set map name that will be used in file names, and 
# to get get boundaries from master NPS list

map <- "test"

# NPS boundaries source: https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True

# Get boundary from OSM
tatry_boundary <- opq(bbox = c(19.8, 49.1, 20.3, 49.3)) %>%
  add_osm_feature(key = "boundary", value = "national_park") %>%
  add_osm_feature(key = "name", value = "Tatrzański Park Narodowy") %>%
  osmdata_sf()

data <- tatry_boundary$osm_multipolygons %>% 
  st_transform(crs = 2180)

d_cent <- data %>% 
  st_union() %>%
  st_centroid() %>% 
  st_transform(crs = 4326)
coords <- st_coordinates(d_cent) %>% as.vector()

# ggplot(data) + geom_sf()

# Example for using specific coordinates to create a geometry, here showing
# coords for the Eye of the Sahara

# data <- st_sfc(st_point(c(-11.401515762405115, 21.127630356438505)), crs = 4326) %>%
#   st_transform(crs = 3081) %>%
#   st_buffer(50000)

# Plot to review
data |>
  ggplot() +
  geom_sf()

################
# Download DEM #
################

# Get DEM using `elevatr` package. Larger Z value (max is 14)
# results in greater resolution. Higher resolution takes more compute, though -- 
# I can't always max `z` up to 14 on my machine. 

z <- 11
zelev <- get_elev_raster(data, z = z, clip = "locations", 
                         src = "aws")  # Using AWS for better European coverage
mat <- raster_to_matrix(zelev)


#Opcjonalne
#res <- mean(round(terra::res(zelev)))

# When initially building your object to render, you'll want to work with
# slimmed down data so you can iterate faster. I prefer to just start with
# a `z` value of 10 above, but an alternative is to create a smaller matrix
# with rayshader::resize_matrix().

# small <- resize_matrix(mat, .25)

# Set up color palette. The `pal` argument will be used in file names,
# so it's important. `colors` will also be passed along. 

pal <- "tatra_alps"

colors <- colorRampPalette(c(
  "#1a4b6d", "#3a7ca5", "#5fa0c7",  # Deep blue shadows
  "#a8d0e6", "#e0f3f8",             # Ice fields
  "#f5f5f5", "#ffffff"              # Pure snow
))(256)

swatchplot(colors)

# Calculate the aspect ratio of the plot so you can translate the dimensions

w <- nrow(mat)
h <- ncol(mat)

# Scale so longer side is 1

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

###################
# Build 3D Object #
###################

# setting shadow to 500 feet below minimum value in DEM
shadow_depth <- min(mat, na.rm = TRUE) - 500

# setting resolution to about 5x for height
res <- mean(round(terra::res(zelev)))

# Keep this line so as you're iterating you don't forget to close the
# previous window

if(rgl::rgl.cur() != 0) rgl::close3d()

# Create the initial 3D object

ambient_layer <- ambient_shade(
  mat,
  zscale = res * 1.5,
  maxsearch = 150,
  sunbreaks = 20,
  anglebreaks = seq(0, 45, 5),
  multicore = parallel::detectCores() - 1
)

mat %>%
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  add_shadow(ambient_layer, 0.4) %>%  # Ambient occlusion
  plot_3d(heightmap = mat, 
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE,
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = res,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = shadow_depth,
          # Set the window size relatively small with the dimensions of our data.
          # Don't make this too big because it will just take longer to build,
          # and we're going to resize with `render_highquality()` below.
          windowsize = c(1200,800), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 17, 
          zoom = 0.45, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 145, 
          background = "#2c2c2c",) 

output_dir <- "C:/Users/Wildbot/Documents/New folder/rayshader_portraits/images/test"

render_snapshot(
  filename = file.path(output_dir, "tatra_mountains_view.png"),
  background = "black",        # Force black background
  width = 2400,                # Higher resolution
  height = 1600,
  instant_capture = TRUE       # Bypass rgl window
)

# Use this to adjust the view after building the window object
render_camera(phi = 17, zoom = 0.45, theta = 145)

###############################
# Create High Quality Graphic #
###############################

# You should only move on if you have the object set up
# as you want it, including colors, resolution, viewing position, etc.

# Ensure dir exists for these graphics
if (!dir.exists(glue("images/{map}"))) {
  dir.create(glue("images/{map}"))
}

# Set up outfile where graphic will be saved.
# Note that I am not tracking the `images` directory, and this
# is because these files are big enough to make tracking them on
# GitHub difficult. 
outfile <- str_to_lower(glue("images/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile,
  coords = coords
), glue("R/portraits/{map}/header.rds"))

c1 <- natparks.pals("Olympic")
c2 <- natparks.pals("Arches")

{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), outfile)
  }
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_status <- tryCatch({
    render_highquality(
      filename = outfile,
      samples = 100,
      light = FALSE,
      # lightdirection = rev(c(160, 160, 165, 165)),
      # lightcolor = c(c1[3], "white", c2[3], "white"),
      # lightintensity = c(500, 75, 75, 150),
      # lightaltitude = c(10, 80, 10, 80),
      environment_light = if(file.exists("assets/env/phalzer_forest_01_4k.hdr")) {
        "assets/env/phalzer_forest_01_4k.hdr"
      } else NULL,
      width = 1500,
      height = 1000,
      parallel = TRUE,
      interactive = FALSE,
      preview = FALSE,
      #ground_material = rayrender::microfacet(roughness = .25)
    )
    TRUE  # Return success
  }, error = function(e) {
    message("Render failed: ", e$message)
    FALSE
  })
  
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}





