library(here)
library(tidyverse)
library(lubridate)
library(tbeptools)
library(sf)

# mean modeled data ---------------------------------------------------------------------------

load(file = here('data/moddat.RData'))

moddat_mean <- moddat |>
  mutate(
    Date = ymd(Date),
    month = month(ymd(Date)),
    year = year(ymd(Date))
  ) |>
  select(dt = Date, seg = modseg, yr = year, mo = month, sal = Salinity,
         chla = Tot_CHL, tn = TN) |>
  group_by(seg, yr, mo) |>
  summarise_if(is.numeric, mean, na.rm = TRUE) |>
  ungroup() |>
  pivot_longer(cols = c(sal, chla, tn), names_to = 'var', values_to = 'val') |>
  mutate(
    dat = 'mod'
  ) |>
  filter(seg < 11)

# compare modeled and observed ----------------------------------------------------------------

##
# assign stations to segments and get observed means

load(file = here("data/modsegs.RData"))

stats <- st_as_sf(stations, coords = c('Longitude', 'Latitude'), crs = 4326) |>
  st_intersection(modsegs) |>
  st_set_geometry(NULL)

obsdat <- epcdata |>
  filter(yr %in% moddat_mean$yr) |>
  select(epchc_station, yr, mo, tn, chla, contains('Sal')) |>
  mutate(
    sal = (Sal_Top_ppth + Sal_Mid_ppth + Sal_Bottom_ppth) / 3
  ) |>
  select(-contains('Sal_')) |>
  left_join(stats, by = 'epchc_station')

obsdat_mean <- obsdat |>
  select(-epchc_station, -bay_segment) |>
  group_by(seg, yr, mo) |>
  summarise_if(is.numeric, mean, na.rm = TRUE) |>
  ungroup() |>
  pivot_longer(cols = c(tn, chla, sal), names_to = 'var', values_to = 'val') |>
  mutate(
    dat = 'obs',
    seg = as.numeric(seg)
  )

##
# combine with modeled and create some plots

tocmp <- bind_rows(moddat_mean, obsdat_mean) |>
  mutate(dt = make_date(yr, mo, 1)) |>
  pivot_wider(names_from = dat, values_from = val)

toplo <- tocmp |>
  filter(seg %in% c(2, 4, 8, 10) & var == 'chla')

ggplot(toplo, aes(x = dt)) +
  geom_line(aes(y = mod, color = 'Modeled')) +
  geom_point(aes(y = obs, color = 'Observed'), size = 0.5) +
  scale_color_manual(values = c('Modeled' = 'blue', 'Observed' = 'black')) +
  theme_minimal() +
  facet_wrap(~seg, ncol = 2, scales = 'free_y') +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = NULL,
    y = 'Chl-a (ug/L)',
    color = NULL
  )


