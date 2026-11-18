# Functions for matching PlanetScope scenes with eddy covariance flux footprints

library(terra)


# Dates
# Convert 20250701_... -> as.Date("2025-07-01")
scene_date <- function(f) as.Date(substr(basename(f), 1, 8), "%Y%m%d")

# as.Date("2025-07-01") -> "jul1" (matches footprint file names)
date_key <- function(d) paste0(tolower(month.abb[as.integer(format(d, "%m"))]),
                               as.integer(format(d, "%d")))



# Clip image and water mask to the fliux footprint, shrunk inward by 3 m
clip_to_fp <- function(r, fp_file, wm, shrink = -3) {
  fp <- project(vect(fp_file), crs(r))
  fp <- buffer(fp, width = shrink)
  if (!all(is.valid(fp))) fp <- makeValid(fp)
  wm <- project(wm, r, method = "near") # reproject + align to image grid
  list(r  = mask(crop(r,  fp), fp),
       wm = mask(crop(wm, fp), fp),
       fp = fp)
}


# Statistics
get_stats <- function(x) {
  v <- values(x, mat = FALSE)
  v <- v[is.finite(v)]
  if (length(v) == 0) return(c(n = 0, mean = NA, median = NA, sd = NA, min = NA, max = NA))
  c(n = length(v), mean = mean(v), median = median(v), sd = sd(v), min = min(v), max = max(v))
}


# Workflow

process_scene <- function(f, fp_dir, wm) {
  d   <- scene_date(f)
  key <- date_key(d)
  fp_file <- file.path(fp_dir, paste0(key, "_.gpkg"))
  if (!file.exists(fp_file)) { message("no footprint, skipping: ", key); return(NULL) }

  x <- clip_to_fp(rast(f), fp_file, wm)

  water <- mask(x$r, x$wm, maskvalues = 0)
  land  <- mask(x$r, x$wm, maskvalues = 1)

  ndvi <- (land[["nir"]] - land[["red"]]) / (land[["nir"]] + land[["red"]])
  idx  <- compute_ps_indices_water(water)

  water_frac <- global(x$wm == 1, "mean", na.rm = TRUE)[1, 1]

  s <- rbind(ndvi_land = get_stats(ndvi),
             t(sapply(names(idx), function(n) get_stats(idx[[n]]))))

  data.frame(date = d, file = basename(f), index = rownames(s),
             water_frac = water_frac, s, row.names = NULL)
}

