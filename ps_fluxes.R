# match planetscope scenes with flux footprints

source("R/util.R")
source("R/util2.R")
source("R/util4.R")


ps_dir  <- "data/ps_scenes"
fp_dir  <- "data/fp_contours"
wm_path <- "data/water_mask_feb26_cropped_corrected.tif"

ps_files <- list.files(ps_dir, pattern = "_SR_8b_harmonized_clip\\.tif$", full.names = TRUE)
wm0 <- rast(wm_path)


# ---- run ----
res <- do.call(rbind, lapply(ps_files, process_scene, fp_dir = fp_dir, wm = wm0))
write.csv(res, "output/ps_footprint_stats.csv", row.names = FALSE)


# ---- optional: visual check of one scene ----
f   <- ps_files[1]
d   <- scene_date(f)
x   <- clip_to_fp(rast(f), file.path(fp_dir, paste0(date_key(d), "_.gpkg")), wm0)
plotRGB(x$r, r = "red", g = "green", b = "blue", stretch = "lin"); lines(x$fp, col = "cyan", lwd = 2)
idx <- compute_ps_indices_water(mask(x$r, x$wm, maskvalues = 0))
plot(idx, col = hcl.colors(120, "Viridis"))
plot_indices(idx, d, paste0("idx_", date_key(d), ".png"))