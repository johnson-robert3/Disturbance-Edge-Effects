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
raw_rhizomes = read_csv("Data/FLK24_rhizomes.csv")

site_data = read_csv("Data/FLK24_site_data.csv")


#- Seagrass -# 

# areal density of variables (seagrass, macroalgae, burrows, etc.)
density = raw_meadow_parameters %>%
   rename(Tt_seed_count = Tt_seed) %>%
   # density (units = shoots / m2)
   mutate(across(ends_with("count"), ~./quadrat_area_m2)) %>%
   rename_with(.cols = ends_with("count"), .fn = ~str_replace(., pattern = "count", replacement = "density")) %>%
   # total macroalgal density
   mutate(total_ma_density = rowSums(select(., Hal_density, Udo_density, Pen_density, Rhi_density, Cau_density), na.rm=TRUE))

# mean blade length and width for each quadrat (morph measured on 5 blades from each quadrat)
morph = raw_morphometry %>%
   # (units = cm)
   summarize(blade_length = mean(blade_length_cm, na.rm=TRUE),
             blade_width = mean(blade_width_cm, na.rm=TRUE),
             .by = c(site_id, treatment, transect_location_m))

# mean blades per shoot for each quadrat (BPS counted on 5 shoots from each quadrat
bps = raw_bps %>%
   summarize(bps = mean(Tt_bps, na.rm=TRUE), .by = c(site_id, treatment, transect_location_m))

# areal aboveground biomass
biomass = raw_ag_biomass %>%
   janitor::remove_empty(which = "rows") %>%
   # areal dry biomass (units = g DM / m2)
   mutate(biomass = (dry_mass_g - dish_mass_g) / quadrat_area_m2) %>%
   # for empty quadrats (no biomass), NAs were entered, replace these with Tt for species so the rows are retained
   mutate(species = replace_na(species, "T. testudinum")) %>%
   select(site_id, treatment, transect_location_m, species, biomass) %>%
   pivot_wider(id_cols = c(site_id, treatment, transect_location_m),
               names_from = species,
               values_from = biomass) %>%
   rename(Tt_biomass = "T. testudinum",
          Hw_biomass = "H. wrightii",
          Sf_biomass = "S. filiforme",
          Hal_biomass = "Halimeda spp.",
          Pen_biomass = "Penicillus spp.",
          Rhi_biomass = "Rhipocephalus spp.",
          drift_biomass = "Drift algae",
          Tt_seed_biomass = "Thalassia seed") %>%
   relocate(Hw_biomass, Sf_biomass, .after = Tt_biomass) %>%
   # total seagrass biomass and total macroalgal biomass in each quadrat
   mutate(total_sg_biomass = rowSums(select(., Tt_biomass, Hw_biomass, Sf_biomass), na.rm=TRUE),
          total_ma_biomass = rowSums(select(., Hal_biomass, Pen_biomass, Rhi_biomass), na.rm=TRUE))


# Rhizomes
rhizomes = raw_rhizomes %>%
   # convert NAs to 0 for cores w/o rhizomes
   mutate(across(contains("mass_g"), ~replace_na(., 0))) %>%
   # areal rhizome biomass (units = g DM / m2)
   mutate(core_area = (pi * (core_diameter_cm^2)) / 10000,
          bg_biomass = (dry_mass_g - dish_mass_g) / core_area) %>%
   # fix the instance where the bottom of the rhizome layer is listed as 0cm, (no rhizomes in core, so should be NA)
   mutate(rhizome_bottom_cm = na_if(rhizome_bottom_cm, 0))


## Combined seagrass parameter dataset
seagrass = density %>%
   select(site_name, site_id, site, patch, treatment, transect_location_m, contains("density")) %>%
   full_join(morph) %>%
   full_join(bps) %>%
   full_join(biomass) %>%
   # add one-sided leaf area index for Thalassia (units = m2 / m2)
   mutate(lai = (Tt_density * blade_length * blade_width * bps) / 10000) %>%
   full_join(rhizomes %>%
                select(site_id, treatment, transect_location_m, rhizome_top_cm, rhizome_bottom_cm, bg_biomass))


#- Sediment -#

sed = raw_spatial_dbd %>%
   mutate(dry_mass = dry_mass_g - wp_mass_g,
          wet_mass = wet_mass_g - wp_mass_g,
          # dry bulk density (units = g / cm3)
          dbd = dry_mass / sample_vol_cc,
          # seawater density = 1.02 g/cc
          porosity = ((wet_mass - dry_mass) / 1.02) / sample_vol_cc,
          om_drymass = dry_sample_g - crucible_mass_g,
          om_ashmass = ashed_mass_g - crucible_mass_g,
          perc_om = (om_drymass - om_ashmass) / om_drymass * 100)


#- Meadow parameter dataset -#

# combine seagrass, sediment, and porewater
meadow = seagrass %>%
   full_join(sed %>%
                select(site_id, treatment, transect_location_m, dbd, porosity, perc_om)) %>%
   full_join(fk_pw_spatial %>%
                select(site_id:sample_depth, porewater_S_uM) %>%
                # widen dataset to make surface and rhizome measurements their own columns (i.e., create one row per sample ID)
                pivot_wider(id_cols = c(site_id:transect_location_m),
                            names_from = sample_depth,
                            values_from = porewater_S_uM) %>%
                rename(surface_pwS = surface,
                       rhizome_pwS = rhizome) %>%
                # update treatment levels to match the seagrass and sediment datasets 
                #  (SG & Sed CSV files had treatment terminology changed later, from dist/undist to unveg/veg)
                mutate(treatment = case_when(treatment == "disturbed" ~ "unvegetated",
                                             treatment == "undisturbed" ~ "vegetated")))



