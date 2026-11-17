# Functions for processing PlanetScope scenes around the flux tower

library(terra)

# Build the 350 x 350 m bounding box around the flux tower, in the raster's CRS
# The tower is NOT centered as to match the observed flux footprint (2010-2025)

make_tower_box <- function(r, lon, lat, L = 350) {
  pt_ll  <- vect(data.frame(x = lon, y = lat), geom = c("x", "y"), crs = "EPSG:4326")
  pt_utm <- project(pt_ll, crs(r))
  xy <- crds(pt_utm, df = TRUE)

  ext(xy$x - 0.75 * L, xy$x + 0.25 * L,
      xy$y - 0.25 * L, xy$y + 0.75 * L)
}

# Keep only water pixels (mask == 1)
mask_to_water <- function(r_box, water_mask) {

  wm <- project(water_mask, r_box, method = "near")

  if (!compareGeom(wm, r_box, stopOnError = FALSE)) {
    stop("Water mask and scene geometry do not match after alignment")
  }

  mask(r_box, wm, maskvalues = 0)
}

# Rename Planet tifs i.e. 20250701_191421_... -> 2025-07-01
scene_date <- function(path) {
  d <- substr(basename(path), 1, 8)
  format(as.Date(d, "%Y%m%d"), "%Y-%m-%d")
}

# Full workflow for one scene  
process_scene <- function(path, water_mask, lon, lat, L = 350) {
  r      <- rast(path)                            # 1. read
  box    <- make_tower_box(r, lon, lat, L)        # 2. bounding box
  r_box  <- crop(r, box)                          # 3. crop
  r_wat  <- mask_to_water(r_box, water_mask)      # 4. water mask
  compute_ps_indices_water(r_wat) library(shiny)  # 5. indices
  library(bslib)
  
  ui <- page_fluid(
    
  )
  
  server <- function(input, output, session) {
  
  }
  
  shinyApp(ui, server)
}

# Save the PNG of the indices side by side
plot_indices <- function(indices, date, file) {
  png(file, width = 1500, height = 500, res = 150)
  par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))
  algae_colors <- colorRampPalette(c("darkorchid4", "seagreen2"))
  for (nm in c("ndci_est", "re10_est", "oc3_est")) {
    plot(indices[[nm]],
         main = paste(toupper(sub("_est", "", nm)), date),
         col  = algae_colors(120))
  }
  dev.off()
}