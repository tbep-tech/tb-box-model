library(tidyverse)
library(lubridate)
library(here)

# prep model data as general output from anlz_avedat ----------------------

# modelled chloropyll
modchl <- read.table(here('data/raw/CHLA observed and predicted 2005-2016.txt'), sep = '\t', header = T) %>%
  mutate(
    var = 'mean_chla',
    Date = mdy(Date)
  ) %>%
  rename(val = Prdctd) %>%
  select(Date, segment = Model_Segment, var, val)

# modelled light attenuation
modlgt <- read.table(here('data/raw/Kd predicted 2005-2016.txt'), sep = '\t', header = T) %>%
  mutate(
    var = 'mean_la',
    Date = ymd(Date),
    ) %>%
  rename(val = Prdcted_Kd) %>%
  select(Date, segment = Model.Segment, var, val)

# combine annual
moddatann <- bind_rows(modchl, modlgt) %>%
  mutate(
    yr = year(Date),
    bay_segment = gsub('^.*\\_(.*$)', '\\1', segment)
  ) %>%
  group_by(yr, bay_segment, var) %>%
  summarise(
    val = mean(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  arrange(var, yr, bay_segment)

# combine mos
moddatmos <- bind_rows(modchl, modlgt) %>%
  mutate(
    yr = year(Date),
    mo = month(Date),
    bay_segment = gsub('^.*\\_(.*$)', '\\1', segment)
  ) %>%
  group_by(bay_segment, yr, mo, var) %>%
  summarise(
    val = mean(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  arrange(var, yr, mo, bay_segment)

moddat <- list(ann = moddatann, mos = moddatmos)

save(moddat, file = here('data/moddat.RData'), version = 2)
