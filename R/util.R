scale_to_reflectance <- function(r){
  mx <- global(r, "max", na.rm = TRUE)[1,1]
  if (!is.na(mx) && mx > 1) r <- r / 10000
  r
}