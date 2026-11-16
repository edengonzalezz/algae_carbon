# R/helpers.R
# Helper functions for processing PlanetScope scenes around the flux tower area


library(terra)

# Build the 350 x 350 m bounding box around the flux tower, in the raster's CRS.
# The tower is NOT centered as to match the observed flux footprint (2010-2025).

make_tower_box <- function(r, lon, lat, L = 350) {
  pt_ll  <- vect(data.frame(x = lon, y = lat), geom = c("x", "y"), crs = "EPSG:4326")
  pt_utm <- project(pt_ll, crs(r))
  xy <- crds(pt_utm, df = TRUE)

  ext(xy$x - 0.75 * L, xy$x + 0.25 * L,
      xy$y - 0.25 * L, xy$y + 0.75 * L)
}

# Keep only water pixels (mask == 1). The mask is aligned to the scene grid first.
mask_to_water <- function(r_box, water_mask) {

  wm <- project(water_mask, r_box, method = "near")

  if (!compareGeom(wm, r_box, stopOnError = FALSE)) {
    stop("Water mask and scene geometry do not match after alignment")
  }

  mask(r_box, wm, maskvalues = 0)
}

# Pull the date out of a Planet filename i.e. "20250701_191421_..." -> "2025-07-01"
scene_date <- function(path) {
  d <- substr(basename(path), 1, 8)
  format(as.Date(d, "%Y%m%d"), "%Y-%m-%d")
}

# Full workflow for one scene. 1. read, 2. crop, 3. water mask, 4. indices
process_scene <- function(path, water_mask, lon, lat, L = 350) {
  r      <- rast(path)
  box    <- make_tower_box(r, lon, lat, L)
  r_box  <- crop(r, box)
  r_wat  <- mask_to_water(r_box, water_mask)
  compute_ps_indices_water(r_wat)
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