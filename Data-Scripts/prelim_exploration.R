#~~~
# Process in situ meadow and site data
#
#~~~


library(tidyverse)

setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance")


# data
raw_spatial_dbd = read_csv("Data/FLK24_spatial_sediment_bulk_density.csv")
raw_meadow_parameters = read_csv("Data/FLK24_meadow_parameters.csv")
raw_morphometry = read_csv("Data/FLK24_thalassia_morphometry.csv")
raw_bps = read_csv("Data/FLK24_thalassia_blades_per_shoot.csv")
raw_ag_biomass = read_csv("Data/FLK24_aboveground_biomass.csv")


# seagrass
density = raw_meadow_parameters %>%
   rename(Tt_seed_count = Tt_seed) %>%
   mutate(across(ends_with("count"), ~./quadrat_area_m2)) %>%
   rename_with(.cols = ends_with("count"), .fn = ~str_replace(., pattern = "count", replacement = "density")) %>%
   mutate(total_ma_density = rowSums(select(., Hal_density, Udo_density, Pen_density)))

morph = raw_morphometry %>%
   summarize(blade_length = mean(blade_length_cm, na.rm=TRUE),
             blade_width = mean(blade_width_cm, na.rm=TRUE),
             .by = c(site_id, treatment, transect_location_m))

bps = raw_bps %>%
   summarize(bps = mean(Tt_bps, na.rm=TRUE), .by = c(site_id, treatment, transect_location_m))

biomass = raw_ag_biomass %>%
   janitor::remove_empty(which = "rows") %>%
   mutate(biomass = (dry_mass_g - dish_mass_g) / quadrat_area_m2) %>%
   mutate(species = replace_na(species, "T. testudinum")) %>%
   summarize(biomass = sum(biomass), .by = c(site_id, treatment, transect_location_m, species)) %>%
   pivot_wider(id_cols = c(site_id, treatment, transect_location_m),
               names_from = species,
               values_from = biomass) %>%
   rename(Tt_biomass = "T. testudinum",
          Hw_biomass = "H. wrightii",
          Hal_biomass = "Halimeda spp.",
          Pen_biomass = "Penicillus spp.",
          drift_biomass = "Drift algae",
          Tt_seed_biomass = "Thalassia seed") %>%
   mutate(total_ma_biomass = rowSums(select(., Hal_biomass, Pen_biomass), na.rm=TRUE))


# sediment
sed = raw_spatial_dbd %>%
   mutate(dry_mass = dry_mass_g - wp_mass_g,
          wet_mass = wet_mass_g - wp_mass_g,
          dbd = dry_mass / sample_vol_cc,
          porosity = ((wet_mass - dry_mass) / 1.02) / sample_vol_cc,
          om_drymass = dry_sample_g - crucible_mass_g,
          om_ashmass = ashed_mass_g - crucible_mass_g,
          perc_om = (om_drymass - om_ashmass) / om_drymass * 100)


# combine
meadow = density %>%
   select(site_id, site, patch, treatment, transect_location_m, contains("density")) %>%
   full_join(morph) %>%
   full_join(bps) %>%
   full_join(biomass) %>%
   full_join(sed %>%
                select(site_id, treatment, transect_location_m, dbd, porosity, perc_om)) %>%
   mutate(transect_location_m = if_else(treatment=="disturbed", transect_location_m * -1, transect_location_m),
          lai = (Tt_density * blade_length * blade_width * bps) / 10000)




