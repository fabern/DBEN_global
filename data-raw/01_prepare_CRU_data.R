# TO BE RUN ON WORKSTATION 02
# WHERE ACCESS TO /data/archiver/cru_harris_2024/data (https://zenodo.org/records/13748398) IS GIVEN
# THEN COPY THE RESULTING FILE INTO subfolder 'data' in this repository

# first: manually unzip the gz files: /data/archive/cru_harris_2024/data $ sudo gzip -dk cru*
# second: then run this script
# last: manually delete again the temporary nc files (only keep the gzipped files)

# options("renv.config.pak.enabled"=TRUE)
# options("renv.config.pak.enabled")
# renv::install("geco-bern/ingestr@adc1bad74a43757155210b127914937b2a9a417d")
library(ingestr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# read in sites to process ----
df_benchmark <- readr::read_csv(
  "data-raw/email_2026-04-01_18h05/Benchmarking_data/TreeMort_D1_3_V1.csv")
df_benchmark |> group_by(Lat,Lon, country) |> summarise(n())

DBEN_sites <- df_benchmark |> distinct(Lat,Lon) |> mutate(
  siteID = sprintf("LON%+08.3f_LAT%+08.3f", Lon,Lat)) |>
  rename(lat = Lat,
         lon = Lon) |>
  mutate(aspect_deg=0, elevation_masl=0, slope_percent=0)

# TODO: get elevation
# TODO: get better CO2
# TODO: ...


# #----------------------------
# extract the CRU time series for the corresponding coordinates  ----
# by looping over all the sites

# Objective is to have:
# > rsofun::biomee_p_model_drivers |> unnest(site_info)
# # A tibble: 1 × 17
#   sitename   lon   lat   elv year_start year_end c4    igbp_land_use plant_functional_type date_start date_end   params_siml       params_tile       params_species     init_cohort       init_soil        forcing
#   <chr>    <dbl> <dbl> <dbl>      <dbl>    <dbl> <lgl> <chr>         <chr>                 <date>     <date>     <list>            <list>            <list>             <list>            <list>           <list>
# 1 CH-Lae    8.36  47.5   700       2004     2014 FALSE Mixed Forests Broadleaf trees       2004-01-01 2014-12-31 <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> <tibble>

# > rsofun::biomee_p_model_drivers |> unnest(forcing)
# # A tibble: 365 × 16
#    sitename site_info         params_siml       params_tile       params_species     init_cohort       init_soil        date         hod    temp       rain   vpd      ppfd   patm  wind   co2
#    <chr>    <list>            <list>            <list>            <list>             <list>            <list>           <date>     <dbl>   <dbl>      <dbl> <dbl>     <dbl>  <dbl> <dbl> <dbl>
#  1 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-01  11.5  0.384  0.0000166   39.5 0.0000571 93092.  3.00  388.
#  2 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-02  11.5 -1.64   0.0000232   40.5 0.0000499 93248.  2.97  388.
#  3 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-03  11.5 -2.51   0.00000371  75.9 0.0000949 93684.  2.84  388.
#  4 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-04  11.5 -1.82   0.0000130   88.0 0.0000706 93435.  2.67  388.
#  5 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-05  11.5 -1.34   0.0000223   67.8 0.0000739 93175.  3.21  388.
#  6 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-06  11.5 -0.450  0.0000219   54.0 0.0000528 93282.  3.03  388.
#  7 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-07  11.5  0.266  0.0000136   64.1 0.0000711 93511.  2.64  388.
#  8 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-08  11.5  0.504  0.0000113   88.2 0.0000905 93443.  2.68  388.
#  9 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-09  11.5  0.0869 0.0000186   59.9 0.0000516 93447.  2.74  388.
# 10 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]> <tibble [1 × 4]> 2009-01-10  11.5 -0.404  0.0000125   58.6 0.0000827 93633.  2.17  388.
# # ℹ 355 more rows
# # ℹ Use `print(n = ...)` to see more rows

# > rsofun::biomee_p_model_drivers |> unnest(params_siml)
# # A tibble: 1 × 18
#   sitename site_info spinup spinupyears recycle firstyeartrend nyeartrend steps_per_day do_U_shaped_mortality update_annualLAImax do_closedN_run method_photosynth method_mortality params_tile params_species
#   <chr>    <list>    <lgl>        <dbl>   <dbl>          <dbl>      <dbl>         <dbl> <lgl>                 <lgl>               <lgl>          <chr>             <chr>            <list>      <list>
# 1 CH-Lae   <tibble>  TRUE           250       1           2009          1             1 TRUE                  TRUE                TRUE           pmodel            dbh              <tibble>    <tibble [16 × 55]>
# # ℹ 3 more variables: init_cohort <list>, init_soil <list>, forcing <list>

# > rsofun::biomee_p_model_drivers |> unnest(params_tile)
# # A tibble: 1 × 26
#   sitename site_info         params_siml soiltype FLDCAP WILTPT    K1    K2 K_nitrogen MLmixRatio  etaN LMAmin fsc_fine fsc_wood GR_factor l_fract retransN f_initialBSW f_N_add tf_base par_mort par_mort_under
#   <chr>    <list>            <list>         <dbl>  <dbl>  <dbl> <dbl> <dbl>      <dbl>      <dbl> <dbl>  <dbl>    <dbl>    <dbl>     <dbl>   <dbl>    <dbl>        <dbl>   <dbl>   <dbl>    <dbl>          <dbl>
# 1 CH-Lae   <tibble [1 × 10]> <tibble>           3    0.4   0.05     2  0.05          8        0.8 0.025   0.02        1        0      0.33       0        0          0.2    0.02       1        1              1
# # ℹ 4 more variables: params_species <list>, init_cohort <list>, init_soil <list>, forcing <list>

# > rsofun::biomee_p_model_drivers |> unnest(params_species)
# # A tibble: 16 × 62
#    sitename site_info params_siml params_tile lifeform phenotype    pt alpha_FR rho_FR root_r root_zeta Kw_root leaf_size   Vmax Vannual wet_leaf_dreg m_cond alpha_phot gamma_L gamma_LN gamma_SW gamma_FR tk_crit
#    <chr>    <list>    <list>      <list>         <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>     <dbl>   <dbl>     <dbl>  <dbl>   <dbl>         <dbl>  <dbl>      <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#  1 CH-Lae   <tibble>  <tibble>    <tibble>           1         0     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  2 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  3 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  4 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  5 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  6 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  7 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  8 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
#  9 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 10 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 11 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 12 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 13 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 14 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 15 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# 16 CH-Lae   <tibble>  <tibble>    <tibble>           1         1     0      1.2    200 2.9e-4      0.29  3.5e-9      0.04 3.5e-5     1.2           0.3      7       0.06    0.02     70.5     0.08       12    283.
# # ℹ 39 more variables: tk_crit_on <dbl>, gdd_crit <dbl>, betaON <dbl>, betaOFF <dbl>, alphaHT <dbl>, thetaHT <dbl>, alphaCA <dbl>, thetaCA <dbl>, alphaBM <dbl>, thetaBM <dbl>, seedlingsize <dbl>,
# #   maturalage <dbl>, v_seed <dbl>, mortrate_d_c <dbl>, mortrate_d_u <dbl>, LMA <dbl>, leafLS <dbl>, LNbase <dbl>, CNleafsupport <dbl>, rho_wood <dbl>, taperfactor <dbl>, lAImax <dbl>, tauNSC <dbl>,
# #   fNSNmax <dbl>, phiCSA <dbl>, CNleaf0 <dbl>, CNsw0 <dbl>, CNwood0 <dbl>, CNroot0 <dbl>, CNseed0 <dbl>, Nfixrate0 <dbl>, NfixCost0 <dbl>, internal_gap_frac <dbl>, kphio <dbl>, phiRL <dbl>, LAI_light <dbl>,
# #   init_cohort <list>, init_soil <list>, forcing <list>

# > rsofun::biomee_p_model_drivers |> unnest(init_cohort)
# # A tibble: 10 × 16
#    sitename site_info params_siml params_tile params_species init_n_cohorts init_cohort_species init_cohort_nindivs init_cohort_bl init_cohort_br init_cohort_bsw init_cohort_bHW init_cohort_seedC init_cohort_nsc
#    <chr>    <list>    <list>      <list>      <list>                  <dbl>               <dbl>               <dbl>          <dbl>          <dbl>           <dbl>           <dbl>             <dbl>           <dbl>
#  1 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  2 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  3 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  4 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  5 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  6 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  7 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  8 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
#  9 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
# 10 CH-Lae   <tibble>  <tibble>    <tibble>    <tibble>                    1                   2                0.05              0              0            0.05               0                 0            0.05
# # ℹ 2 more variables: init_soil <list>, forcing <list>

# > rsofun::biomee_p_model_drivers |> unnest(init_soil)
# # A tibble: 1 × 11
#   sitename site_info         params_siml       params_tile       params_species     init_cohort       init_fast_soil_C init_slow_soil_C init_Nmineral N_input forcing
#   <chr>    <list>            <list>            <list>            <list>             <list>                       <dbl>            <dbl>         <dbl>   <dbl> <list>
# 1 CH-Lae   <tibble [1 × 10]> <tibble [1 × 11]> <tibble [1 × 19]> <tibble [16 × 55]> <tibble [10 × 9]>             0.01            0.001         0.015  0.0008 <tibble [365 × 9]>

### Create siteinfo (and complement with Water Holding Capacity (unneeded for BiomeE)) ----
siteinfo <- DBEN_sites |>
  select(sitename=siteID, lon, lat, elv = elevation_masl) |>
  mutate(year_start = 1901,
         year_end   = 2015)

#FOR DEVELOPMENT
set.seed(123)
siteinfo <- siteinfo |> dplyr::slice_sample(n=2)

n_sites <- nrow(siteinfo)

## Get CRU forcing datasets for BiomeE with ingestr ----
Sys.getenv("")
### Meteorological forcing from CRU data (https://crudata.uea.ac.uk/cru/data/hrg/)
df_meteo_cru <- ingestr::ingest(
  siteinfo = siteinfo,
  source = "cru",
  getvars = c("temp", "prec", "ppfd", "vpd", "ccov", "patm"),     #TODO: add tmin, tmax (if needed) ### "dtr" dtr=diurnal temperature range
  dir = ifelse(grepl(pattern = "cnode[0-9]*", as.character(Sys.info()["nodename"])),
               "/storage/capacity/occr_geco/data/archive/cru_harris_2024/data/", # base path on UBELIX
               "/data/archive/cru_harris_2024/data/"), # base path on workstation
  timescale = "d",
  settings = list(correct_bias = NULL)
  # settings = list(correct_bias = "worldclim", dir_bias = "/data/archive/worldclim_fick_2017/data/")  # TODO: why are we limiting the CRU data extraction. a) Couldn't we just apply the 12 month-based bias corrections to the full period. b) Why is it saying it stops at 2000, but 2012-2016 is feasible in the CRU vignette?
) |> tidyr::unnest(data)


### Get CO2 data from Mauna Loa ----
df_co2 <- read.csv(
  url("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv"), # or use ingestr, source="co2_mlo"
  skip = 43) |>
  dplyr::select(year, mean) |>
  dplyr::rename(co2 = mean)

df_meteo_cru <- df_meteo_cru |>
  dplyr::mutate(year = lubridate::year(date)) |>
  # merge CO2 by year
  dplyr::left_join(df_co2, by = "year") |>
  # for years before Mauna Loa measurements started (1959), use the earliest value (1959 = 315.98 ppm)
  tidyr::fill(co2, .direction = "up")

# write all climate data from CRS to file
basepath <- paste0("DBEN_CRU_",n_sites,"sites_meteo_data")
saveRDS(
  df_meteo_cru,
  paste0("/data_2/scratch/fbernhard/",basepath,".rds"),
  compress = "xz"
) # and then manually copy this to DBEN_global/data/DBEN_CRU_meteo_data.rds
# df_meteo_cru <- readRDS(paste0("/data_2/scratch/fbernhard/",basepath,".rds"))
# df_meteo_cru <- readRDS(paste0("../DBEN_global/data/",basepath,".rds"))

# Illustrate the meteo data ----
library(ggplot2)
plot1 <- df_meteo_cru |> pivot_longer(!c("date","sitename")) |>
  ggplot(aes(x=date, y = value, color = sitename)) + geom_line() + facet_grid(name~., scales = "free_y") +
  theme_bw() +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%b\n%Y")
max_year <- lubridate::year(max(df_meteo_cru$date))
plot2 <- df_meteo_cru |> pivot_longer(!c("date","sitename")) |>
  mutate(doy   = lubridate::yday(date),
         month = lubridate::month(date),
         year  = lubridate::year(date) ) |>
  mutate(decade = cut(year, breaks = c(seq(1900,max_year, by = 20), max_year), dig.lab = 4)) |>
  ggplot(aes(x=doy, y = value, color = sitename, group = interaction(sitename, year))) +
  geom_line(alpha = 0.3) +
  facet_grid(name~decade, scales = "free_y", switch = "y") +
  scale_x_continuous("", breaks = seq(1, 360, by = 28), #labels = ~lubridate::month(.x, label = TRUE),
                     minor_breaks = seq(1, 365, by = 7)) +
  scale_y_continuous("") +
  theme_bw() + theme(strip.placement = "outside")

ggsave(
  plot1,width = 3200, height = 2400, units = "px",
  filename = paste0("/data_2/scratch/fbernhard/",basepath,"1_PROFOUNDsites.png"))
ggsave(
  plot2,width = 3200, height = 2400, units = "px",
  filename = paste0("/data_2/scratch/fbernhard/",basepath,"2_PROFOUNDsites.png"))


#
#
# ### Get fAPAR with ??? ----
# # Set fAPAR by default to 1 for all observations, which is the same as what
# #`ingest(siiteinfo_globresp, source = "fapar_unity")` would do. This makes
# #sense for leaf-level simulations.
# # TODO: But not for BiomeE simulations.
# # TODO: for BiomeE get fapar!
#
# df_meteo_cru <- df_meteo_cru |>
#   # Add fapar column with value 1
#   dplyr::mutate(fapar = 1) |>                     # TODO: improve this assumption
#   # Add wind column with value 1 m/s
#   dplyr::mutate(wind = 1) # m/s, assume 1 m/s     # TODO: improve this assumption
#
#
# ## Put all data together into rsofun driver object ----
# # Define default model parameters, soil data, etc
# params_siml_biomee <- list(
#   spinup                = TRUE,
#   spinupyears           = 1901,
#   recycle               = 50,
#
#   firstyeartrend        = 1901,
#   nyeartrend            = 100,
#   steps_per_day         = 1,
#   do_U_shaped_mortality = TRUE,
#   update_annualLAImax   = TRUE,
#   do_closedN_run        = TRUE,
#   method_photosynth     = "pmodel", #"gs_leuning", # "pmodel"
#   method_mortality      = "dbh")
#
# drivers <- df_meteo_cru |>
#   # nest forcing
#   nest(forcing = !c(sitename)) |>
#   ungroup() |>
#   # add site_info
#   left_join(nest(siteinfo |> mutate(whc = 432),                        # TODO: improve this assumption
#                  site_info = !c(sitename)),
#             by = "sitename")
#
#             # # NOTE: for p-model: forcing needs to have the following:
#             # # select(date,temp,vpd,ppfd,netrad,patm,snow,
#             # #        rain,tmin,tmax,fapar,co2,ccov)
#             # # drivers$forcing[[1]] |>
#             # #   mutate(netrad=000, snow=000) |>
#             # #   dplyr::select(date,temp,vpd,ppfd,netrad,patm,snow,
#             # #                 rain,tmin,tmax,fapar,co2,ccov)
#             # df_soiltexture <- tribble(~layer, ~fsand, ~fclay, ~forg, ~fgravel,
#             #                           "top",   0.4,    0.3,    0.1,   0.1,
#             #                           "bottom",0.4,    0.3,    0.1,   0.1)
#             # drivers_pmodel <- drivers |>
#             #   # add params_siml
#             #   mutate(params_siml = list(as_tibble(params_siml_pmodel))) |>
#             #   # add params_soil
#             #   mutate(params_soil = list(df_soiltexture)) |>
#             #   mutate(forcing = purrr::map(forcing, \(df_frc) df_frc |>
#             #                                 mutate(netrad=000, snow=000) |>
#             #                                 rename(rain=prec) |>
#             #                                 dplyr::select(date, temp,vpd,ppfd,netrad,patm,snow,
#             #                                               rain,tmin,tmax,fapar,co2,ccov)
#             #   ))
#             # res_pmodel <- runread_pmodel_f(drivers = drivers_pmodel,
#             #                                par = list(
#             #                                  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
#             #                                  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
#             #                                  kphio_par_b        = 1.0,
#             #                                  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
#             #                                  soilm_betao        = 0.0,
#             #                                  beta_unitcostratio = 146.0,
#             #                                  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#             #                                  tau_acclim         = 30.0,
#             #                                  kc_jmax            = 0.41),
#             #                                makecheck = TRUE)
#
# # NOTE: for BiomeE model: forcing needs to have the following: hod = 11.5, wind = 1.0 m/s
# drivers_biomee <- drivers |>
#   # add params_siml
#   mutate(params_siml = list(as_tibble(params_siml_biomee))) |>
#   # add params_tile
#   mutate(params_tile = list(as_tibble(rsofun::biomee_gs_leuning_drivers$params_tile[[1]]))) |>       # TODO: define this for PROFOUND
#   # add params_species
#   mutate(params_species = list(as_tibble(rsofun::biomee_gs_leuning_drivers$params_species[[1]]))) |> # TODO: define this for PROFOUND
#   # add init_cohort
#   mutate(init_cohort = list(as_tibble(rsofun::biomee_gs_leuning_drivers$init_cohort[[1]]))) |>       # TODO: define this for PROFOUND
#   # add init_soil
#   mutate(init_soil = list(as_tibble(rsofun::biomee_gs_leuning_drivers$init_soil[[1]]))) |>           # TODO: define this for PROFOUND
#   mutate(forcing = purrr::map(forcing, \(df_frc) df_frc |>
#                                 mutate(wind=1.0,    # just set wind to 1 m/s
#                                        hod=11.5) |>
#                                 rename(rain=prec) |>
#                                 dplyr::select(date,hod,temp,rain,
#                                               vpd,ppfd,patm,wind,co2)
#   ))
#
# # Illustrate the input data ----
# library(ggplot2)
# df_CRU_PROFOUND2 <- drivers_biomee |> select(sitename, forcing)
# plot1 <- unnest(df_CRU_PROFOUND2, forcing) |> pivot_longer(!c("date","sitename")) |>
#   ggplot(aes(x=date, y = value, color = sitename)) + geom_line() + facet_grid(name~., scales = "free_y") +
#   theme_bw() +
#   scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", date_labels = "%b\n%Y")
# max_year <- lubridate::year(max(unnest(df_CRU_PROFOUND2, forcing)$date))
# plot2 <- unnest(df_CRU_PROFOUND2, forcing) |> pivot_longer(!c("date","sitename")) |>
#   mutate(doy   = lubridate::yday(date),
#          month = lubridate::month(date),
#          year  = lubridate::year(date) ) |>
#   mutate(decade = cut(year, breaks = c(seq(1900,max_year, by = 20), max_year), dig.lab = 4)) |>
#   ggplot(aes(x=doy, y = value, color = sitename, group = interaction(sitename, year))) +
#   geom_line(alpha = 0.3) +
#   facet_grid(name~decade, scales = "free_y", switch = "y") +
#   scale_x_continuous("", breaks = seq(1, 360, by = 28), #labels = ~lubridate::month(.x, label = TRUE),
#                      minor_breaks = seq(1, 365, by = 7)) +
#   scale_y_continuous("") +
#   theme_bw() + theme(strip.placement = "outside")
#
# ggsave(
#   plot1,width = 3200, height = 2400, units = "px",
#   filename = "/data_2/scratch/fbernhard/DBEN_CRU_biomee_forcing_data1_PROFOUNDsites.png")
# ggsave(
#   plot2,width = 3200, height = 2400, units = "px",
#   filename = "/data_2/scratch/fbernhard/DBEN_CRU_biomee_forcing_data2_PROFOUNDsites.png")
#
#
#
# # write all climate data from biomee driver to file
# saveRDS(
#   df_CRU_PROFOUND2,
#   "/data_2/scratch/fbernhard/DBEN_CRU_biomee_forcing_data_PROFOUNDsites.rds",
#   compress = "xz"
# ) # and then manually copy this to MIND_biomee_calibration/data/MIND_rsofun_driver_data_v3.4.rds
# saveRDS(
#   drivers_biomee,
#   "/data_2/scratch/fbernhard/DBEN_CRU_meteo_data.rds",
#   compress = "xz")
#
#
# # Run BiomeE ----
# library(rsofun)
# res_biomee_PRECURSOR <- runread_biomee_f(drivers = drivers_biomee[1:9,], makecheck = TRUE)
# res_biomee <- res_biomee_PRECURSOR |> unnest_wider(data, simplify = FALSE)
#
# # Plot BiomeE outputs ----
# for (site_iter in seq_along(res_biomee$sitename)){
#   site_iter_name <- res_biomee$sitename[site_iter]
#   print(site_iter_name)
#
#   plot1_biomee <- res_biomee$output_daily_tile[[site_iter]] |> tibble() |>
#     mutate(date = lubridate::make_date(year = year) + lubridate::days(doy - 1)) |>
#     select(date, year, doy, Tk, Prcp, totWs, Trsp, Evap, Runoff, LAI, GPP, NSC, leafC, rootC, SW_C, HW_C, fastSOM, slowSOM) |>
#     tidyr::pivot_longer(!c(date, year, doy)) |>
#     ggplot(aes(x = date, y=value, color = name)) +
#     geom_line() + facet_grid(name~., scales = "free_y") + theme_bw()
#   plot2_biomee <- res_biomee$output_annual_tile[[site_iter]] |> tibble() |>
#     mutate(date = lubridate::make_date(year = year)) |>             # TODO: homogenize Transp to Trsp, ...
#     select(date, year, CAI, LAI, Density, DBH, DBH12, NPP, GPP, Rh, Transp, Evap, plantC) |>
#     tidyr::pivot_longer(!c(date, year)) |>
#     ggplot(aes(x = date, y=value, color = name)) +
#     geom_line() + facet_grid(name~., scales = "free_y") + theme_bw()
#   ggsave(
#     plot1_biomee,width = 3200, height = 2400, units = "px",
#     filename = paste0("/data_2/scratch/fbernhard/DBEN_CRU_biomee_drivers_result1_PROFOUNDsites_",site_iter,"_",site_iter_name,".png"))
#   ggsave(
#     plot2_biomee,width = 3200, height = 2400, units = "px",
#     filename = paste0("/data_2/scratch/fbernhard/DBEN_CRU_biomee_drivers_result2_PROFOUNDsites_",site_iter,"_",site_iter_name,".png"))
# }
#
#
# # CLEANUP
# # and then manually delete all the temporary nc files in : /data/archive/cru_harris_2024/data $ sudo rm cru_ts4.08.1901.2023.*.nc
#
#
#
# # TODO: if we extract a forcing period that is shorter than the recycle period, we get no error from biomee, but get NA as output. #TODO: make issue out of this
#
#


