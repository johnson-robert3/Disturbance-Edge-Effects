#
# Playing with data
#


library(tidyverse)

setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance/Data")


# data
raw_spatial_dbd = read_csv("FLK24_spatial_sediment_bulk_density.csv")
raw_meadow_parameters = read_csv("FLK24_meadow_parameters.csv")
raw_morphometry = read_csv("FLK24_thalassia_morphometry.csv")
raw_bps = read_csv("FLK24_thalassia_blades_per_shoot.csv")
raw_ag_biomass = read_csv("FLK24_aboveground_biomass.csv")


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
   mutate(biomass = dry_mass_g - dish_mass_g) %>%
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
          porosity = ((wet_mass - dry_mass) / 1.02) / sample_vol_cc)



# combine

test = density %>%
   select(site_id, treatment, transect_location_m, contains("density")) %>%
   full_join(morph) %>%
   full_join(bps) %>%
   full_join(biomass) %>%
   full_join(sed %>%
                select(site_id, treatment, transect_location_m, dbd, porosity)) %>%
   mutate(transect_location_m = if_else(treatment=="disturbed", transect_location_m * -1, transect_location_m),
          lai = (Tt_density * blade_length * blade_width * bps) / 10000)





# Figures

# Thalassia shoot density 
windows(); ggplot(test, aes(x = transect_location_m, y = Tt_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Thalassia blade length 
windows(); ggplot(test, aes(x = transect_location_m, y = blade_length, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   ylim(0, 45) +
   theme_bw()


# Thalassia biomass
windows(); ggplot(test, aes(x = transect_location_m, y = Tt_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Thalassia LAI 
windows(); ggplot(test, aes(x = transect_location_m, y = lai, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Macroalgal density 
windows(); ggplot(test, aes(x = transect_location_m, y = total_ma_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Macroalgal biomass 
windows(); ggplot(test, aes(x = transect_location_m, y = total_ma_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Drift algae biomass 
windows(); ggplot(test, aes(x = transect_location_m, y = drift_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Sediment bulk density 
windows(); ggplot(test, aes(x = transect_location_m, y = dbd, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Small burrows
windows(); ggplot(test, aes(x = transect_location_m, y = Burrow_sm_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Large burrows
windows(); ggplot(test, aes(x = transect_location_m, y = Burrow_lg_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()



