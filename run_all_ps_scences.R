# run_all_scenes.R
# Process every PlanetScope GeoTIFF in a folder: crop to flux tower box,
# mask to water, compute NDCI / RE10 / OC3, and save results.

library(terra)
source("R/util2.R")   # compute_ps_indices_water()
source("R/util3.R")   # make_tower_box(), mask_to_water(), process_scene(), and more 


in_dir    <- "data/ps_scenes"         
mask_file <- "data/water_mask_feb26_cropped_corrected.tif"
out_dir   <- "output"
tower_lon <- -121.7650
tower_lat <-   38.0499
box_size  <- 350                          

# Set up
dir.create(file.path(out_dir, "tifs"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

water_mask <- rast(mask_file)   # read once, reused for every scene
scenes <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
message("Found ", length(scenes), " scenes")

# Loop over all .tiff files
for (path in scenes) {
  date <- scene_date(path)
  message("Processing ", date)

  # tryCatch so one bad scene doesn't stop the whole run
  tryCatch({
    indices <- process_scene(path, water_mask, tower_lon, tower_lat, box_size)

    writeRaster(indices,
                file.path(out_dir, "tifs", paste0("indices_", date, ".tif")),
                overwrite = TRUE)
    plot_indices(indices, date,
                 file.path(out_dir, "plots", paste0("indices_", date, ".png")))
  }, error = function(e) {
    message("  FAILED: ", basename(path), " -> ", conditionMessage(e))
  })
}

message("Done. Results in ", out_dir)