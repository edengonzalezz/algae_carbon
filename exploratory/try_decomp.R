# first update date columns

library(tidyverse)
library(astsa)
library(zoo)


# Load data 
ameriflux <- read_csv("data/AMF_US-Myb_BASE_HH_16-5.csv", skip=2, na = c("-9999", "-9999.0"))
ps_fp <- read_csv("output/ps_footprint_stats.csv")

# July-August 2025, match PS overpass time (11:00-11:30 PST)
fluxes <- ameriflux |> 
  select(TIMESTAMP_START, TIMESTAMP_END, FC, FCH4) |>
    rename(
    date = DATE, 
    timestamp_start = TIMESTAMP_START,
    timestamp_end = TIMESTAMP_END,
    fc = FC,
    fch4 = FCH4) |> 
  mutate(
    timestamp_start = ymd_hm(as.character(timestamp_start)),
    timestamp_end = ymd_hm(as.character(timestamp_end))
  ) |> 
  filter(
    timestamp_start >= ymd("2025-07-01"), 
    timestamp_end <= ymd("2025-08-31"), 
    hour(timestamp_start) == 11,
    minute(timestamp_start) == 0, 
    minute(timestamp_end) == 30
  ) |> 
  mutate(date = as_date(timestamp_start)) |> 
  select(date, timestamp_start, timestamp_end, fc, fch4) 

  

ndci_only <- ps_fp |> 
  filter(index == "ndci_est") |> 
  mutate(date = as_date(date))


ndci_ts <- ts(na.approx(ndci_only$median, na.rm = FALSE), frequency = 7)
ndciComp <- decompose(ndci_ts)
plot(ndciComp)


ndciSeasonAdj <- ndci_ts - ndciComp$seasonal
plot.ts(ndciSeasonAdj) # trend + random component
plot.ts(ndci_ts) # original data 
plot.ts(ndciComp$seasonal) # what we subtracted from original data




re10_only <- ps_fp |> 
  filter(index == "re10_est") |> 
  mutate(date = as_date(date))


re10_ts <- ts(na.approx(re10_only$median, na.rm = FALSE), frequency = 7)
re10Comp <- decompose(re10_ts)
plot(re10Comp)


re10SeasonAdj <- re10_ts - re10Comp$seasonal
plot.ts(re10SeasonAdj) # trend + random component
plot.ts(re10_ts) # original data 
plot.ts(re10Comp$seasonal) # what we subtracted from original data




oc3_only <- ps_fp |> 
  filter(index == "oc3_est") |> 
  mutate(date = as_date(date))


oc3_ts <- ts(na.approx(oc3_only$median, na.rm = FALSE), frequency = 7)
oc3Comp <- decompose(oc3_ts)
plot(oc3Comp)


oc3SeasonAdj <- oc3_ts - oc3Comp$seasonal
plot.ts(oc3SeasonAdj) # trend + random component
plot.ts(oc3_ts) # original data 
plot.ts(oc3Comp$seasonal) # what we subtracted from original data



ggplot(ndci_only, aes(x = date, y = median))+
  geom_line() +
  theme_bw()

