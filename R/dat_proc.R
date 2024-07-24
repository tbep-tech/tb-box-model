library(here)
library(tidyverse)
library(data.table)
library(lubridate)
library(tbeptools)
library(sf)

# get raw model output, minimal formatting ----------------------------------------------------

# Import model output as txt file in wide format, without column headers:
datraw <- fread("C:/box_model/test_run/V6_BM_SM36.txt", skip=0, header = FALSE)

moddat <- datraw |> set_names(c(    'Station_ID',
                                 'Sim_Day',
                                 'Seg_Depth_m',
                                 'W_Temp_C',
                                 'Wind_mps',
                                 'Salinity',
                                 'TotLght',
                                 'Blank1',
                                 'Blank2',
                                 'Ke_Tot',
                                 'Ke_Back',
                                 'Ke_Phyto',
                                 'Blank3',
                                 'Blank4',
                                 'NH4_N',
                                 'NO3_N',
                                 'DIN',
                                 'DON',
                                 'PON',
                                 'TON',
                                 'TN',
                                 'TKN',
                                 'DIP',
                                 'DOP',
                                 'POP',
                                 'TOP',
                                 'TP',
                                 'Blank5',
                                 'Phyto_C',
                                 'Tot_CHL',
                                 'Phy_PtntlGrowth',
                                 'Blank6',
                                 'Phy_Resp',
                                 'PhyLossZooGr',
                                 'Phy_Settl',
                                 'Phy_GPP',
                                 'Phy_NPP',
                                 'CCHL_ratio',
                                 'Phyt_LghtLim',
                                 'Phyt_NutrLim',
                                 'Phyt_Lim',
                                 'Phy_ActlGrowth',
                                 'CBOD',
                                 'BOD5',
                                 'BOD_Decay',
                                 'DO_mgl',
                                 'DO_Sat_mgl',
                                 'DO_Prcnt_Sat',
                                 'Effec_KA',
                                 'Wind_KA',
                                 'Hydr_KA',
                                 'Blank7',
                                 'Blank8',
                                 'Detritus_C',
                                 'POC2SD',
                                 'POP2SD',
                                 'PON2SD',
                                 'SOD',
                                 'NH4_Flx',
                                 'PO4_Flx')) |>
  as_tibble() |>
  mutate(
    Date = make_date(year=1985, month=1, day=1),
    Date = (Date + Sim_Day)
  ) |>
  rename(modseg = Station_ID)

save(moddat, file = here('data/moddat.RData'))

# get annual and monthly sums of model data ---------------------------------------------------

load(file = here('data/moddat.RData'))

dat <- moddat |>
  filter(modseg < 11) |>
  mutate(
    bay_segment = case_when(
      modseg %in% c(1:3) ~ 'OTB',
      modseg %in% c(4:5) ~ 'HB',
      modseg %in% c(6:8) ~ 'MTB',
      modseg %in% c(9:10) ~ 'LTB'
    ),
    Date = ymd(Date),
    month = month(ymd(Date)),
    year = year(ymd(Date))
  ) |>
  select(dt = Date, bay_segment, modseg, yr = year, mo = month,
         mean_chla = Tot_CHL, mean_la = Ke_Tot)

moddatmo <- dat |>
  summarise(
    mean_chla = mean(mean_chla, na.rm = T),
    mean_la = mean(mean_la, na.rm = T),
    .by = c('bay_segment', 'yr', 'mo', 'modseg')
  ) |>
  pivot_longer(cols = c('mean_chla', 'mean_la'), names_to = 'var', values_to = 'val') |>
  summarise(
    val = mean(val, na.rm = T),
    .by = c('bay_segment', 'yr', 'mo', 'var')
  )

moddatann <- moddatmo |>
  summarise(
    val = mean(val, na.rm = T),
    .by = c('bay_segment', 'yr', 'var')
  )

moddatannmos <- list(
  ann = moddatann,
  mos = moddatmo
)

save(moddatannmos, file = here('data/moddatannmos.RData'), version = 2)

# get box model segments ----------------------------------------------------------------------

shps <- list.files('T:/05_GIS/WASP_MODEL/Morrison_box_model/', pattern = 'shp$', full.names = T)
shps <- grep('fixed', shps, value = T)

modsegs <- tibble::tibble(
  seg = shps
) |>
  dplyr::group_by(seg) |>
  tidyr::nest(.key = 'dat') |>
  dplyr::mutate(
    dat = purrr::map(seg, ~ .x |>
                       st_read() |>
                       st_union() |>
                       st_sf() |>
                       st_buffer(dist = 10) |>
                       st_buffer(dist = -10) |>
                       st_simplify(dTolerance = 10) |>
                       st_transform(crs = 4326))
  ) |>
  ungroup() |>
  unnest('dat') |>
  st_sf() |>
  mutate(
    seg = gsub('^.*\\s(\\d+).*', '\\1', basename(shps))
  ) |>
  st_make_valid()

save(modsegs, file = here('data/modsegs.RData'))
