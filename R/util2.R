source("R/util.R")

# Create a function to apply spectral algorithms (oc3, ndci, re10) 

compute_ps_indices_water <- function(
    ps_image,
    min_reflectance = 1e-3,
    oc3_coeffs   = c(a = 0.3483, b = -2.9959, c = 2.9873, d = -1.4813, e = -0.0597),
    eps          = 1e-6
){
  
  
  # Scale to 0–1 reflectance because 0–10000 DNs
  ps_image <- scale_to_reflectance(ps_image)
  

  # Extract individual bands
  coastal_blue <- ps_image[["coastal_blue"]] # ~443 nm
  blue <- ps_image[["blue"]]                 # ~490 nm
  green <- ps_image[["green"]]               # ~565 nm
  red  <- ps_image[["red"]]                  # ~665 nm
  re   <- ps_image[["rededge"]]              # ~705–709 nm
  nir  <- ps_image[["nir"]]                  # ~865 nm
  
  # Use NIR band to mask out land 
  nir_clean <- !is.na(nir)  #  (nir <= t_nir) & is.finite(nir) & (nir > min_reflectance)
  
  # Define acceptable reflectance
  valid_base <- is.finite(coastal_blue) & is.finite(blue) & is.finite(green) &
    is.finite(red)  & is.finite(re)   &
    (coastal_blue > min_reflectance) & (blue > min_reflectance) &
    (green > min_reflectance) & (red  > min_reflectance) &
    (re   > min_reflectance)
  
  valid <- nir_clean & valid_base
  
  
  # Turn into logical 0/1 spatraster so mask() behaves consistently

  valid_water <- ifel(valid, 1, 0)
  
  
  # calculate ndci
  
  # ndci = (re - red) / (re + red)
  denom_ndci <- re + red
  denom_ndci <- ifel(denom_ndci < min_reflectance, NA, denom_ndci)
  
  ndci <- (re - red) / (denom_ndci + eps)
  ndci <- mask(ndci, valid_water, maskvalues = 0, updatevalue = NA)
  names(ndci) <- "ndci_est"
  
  # calculate re10 
  # https://www.mdpi.com/2077-1312/10/8/1104
  # r2 = re/red, inner = 35.75*nir - 14.30, re10 = inner^1.124 (if inner > 0)
  
  r2    <- re / (red + eps)
  inner <- 35.75 * r2 - 14.30
  
  re10 <- ifel(valid & (inner > 0), inner^1.124, NA)
  names(re10) <- "re10_est"

  
  # calculate oc3
  # https://onlinelibrary.wiley.com/doi/abs/10.1029/98JC02160  pg. 24,946
  # numerator = max(coastal_blue, blue)
  num <- mosaic(coastal_blue, blue, fun = "max")
  den <- green
  
  num <- ifel(num < min_reflectance, NA, num)
  den <- ifel(den < min_reflectance, NA, den)
  
  ratio <- num / (den + eps)
  ratio <- mask(ratio, valid_water, maskvalues = 0, updatevalue = NA)
  
  X <- log10(ratio)
  a <- oc3_coeffs[1]; b <- oc3_coeffs[2]; c <- oc3_coeffs[3]
  d <- oc3_coeffs[4]; e <- oc3_coeffs[5]
  
  oc3 <- 10^(a + b*X + c*X^2 + d*X^3 + e*X^4)
  names(oc3) <- "oc3_est"

  # stack all three to  compare directly
  out <- c(ndci, re10, oc3)
  out

}
