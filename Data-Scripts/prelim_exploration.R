#
# Playing with data
#


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




# Figures

# Thalassia shoot density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = Tt_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Tt_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Tt_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Thalassia blade length 
windows(); ggplot(meadow, aes(x = transect_location_m, y = blade_length, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   ylim(0, 45) +
   theme_bw()


# Thalassia biomass
windows(); ggplot(meadow, aes(x = transect_location_m, y = Tt_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Tt_biomass, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Tt_biomass, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Thalassia LAI 
windows(); ggplot(meadow, aes(x = transect_location_m, y = lai, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Macroalgal density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = total_ma_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = total_ma_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = total_ma_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Macroalgal biomass 
windows(); ggplot(meadow, aes(x = transect_location_m, y = total_ma_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = total_ma_biomass, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = total_ma_biomass, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Drift algae biomass 
windows(); ggplot(meadow, aes(x = transect_location_m, y = drift_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()



#-- Sediments --# 

# Sediment bulk density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = dbd, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = dbd, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = dbd, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   


# Sediment percent OM
windows(); ggplot(meadow, aes(x = transect_location_m, y = perc_om, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = perc_om, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = perc_om, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Small burrows
windows(); ggplot(meadow, aes(x = transect_location_m, y = Burrow_sm_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Burrow_sm_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Burrow_sm_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Large burrows
windows(); ggplot(meadow, aes(x = transect_location_m, y = Burrow_lg_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()



