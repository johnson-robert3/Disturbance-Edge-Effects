#
# Figures for viewing seagrass and sediment data variables
# 

library(tidyverse)


# run 'data_spatial-porewater-S' and 'data_meadow-data' scripts first to generate data

# add a new variable for graphing distance along the transect
meadow = meadow %>%
   mutate(distance = if_else(treatment=="unvegetated", transect_location_m * -1, transect_location_m))



#- Organic Matter viewed along transects -# 

# OM based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = perc_om, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = perc_om), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(OM~('%'))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# OM vs distance from the veg edge
#  all data pooled, separated by treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = perc_om, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = perc_om, color = treatment), method="lm", se=F)

# with separate lines by site
#  all data pooled, separated by site and treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = perc_om, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = perc_om, color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)
## OM at the 5m mark within bare patches is similar across sites (OM seems to converge near the middle of bare patches), but is more variable
##  within bare patches next to the seagrass boundary (and sometimes similar to the OM in the seagrass area near the edge)


# OM, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(perc_om, na.rm=T), se = se(perc_om), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "%OM") +
   theme_classic()



#- Sediment Bulk Density viewed along transects -#

# DBD based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = dbd, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = dbd), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Bulk~density~(g~cm^-3))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# DBD vs distance from the veg edge
#  all data pooled, separated by treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = dbd, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = dbd, color = treatment), method="lm", se=F)

# with separate lines by treatment
#  all data pooled, separated by site and treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = dbd, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = dbd, color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)


# DBD, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(dbd, na.rm=T), se = se(dbd), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "Dry bulk density") +
   theme_classic()



#- Sediment Porosity viewed along transects -#

# Porosity based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = porosity, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = porosity), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Porosity)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Porosity vs distance from the veg edge
#  all data pooled, separated by treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = porosity, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = porosity, color = treatment), method="lm", se=F)

# with separate lines by treatment
#  all data pooled, separated by site and treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = porosity, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = porosity, color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)


# Porosity, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(porosity, na.rm=T), se = se(porosity), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "Porosity") +
   theme_classic()



#- Thalassia seagrass parameters viewed along transects -#

# Tt aboveground biomass based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow %>%
          mutate(Tt_biomass = if_else(treatment=="unvegetated" & is.na(Tt_biomass), 0, Tt_biomass))) +
   #
   geom_line(aes(x = distance, y = Tt_biomass, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = Tt_biomass), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Thalassia~biomass), limits = c(0,300)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Tt Shoot Density based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = Tt_density, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = Tt_density), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Thalassia~shoot~density)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Tt canopy height (leaf length)
windows(height=3.5, width=8)
ggplot(meadow %>%
          mutate(blade_length = if_else(treatment=="unvegetated" & is.na(blade_length), 0, blade_length))) +
   #
   geom_line(aes(x = distance, y = blade_length, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = blade_length), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Seagrass~canopy~height~(cm))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Leaf Area Index
windows(height=3.5, width=8)
ggplot(meadow %>%
          mutate(lai = if_else(treatment=="unvegetated" & is.na(lai), 0, lai))) +
   #
   geom_line(aes(x = distance, y = lai, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = lai), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(LAI)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome biomass (just at the 0.5 and 5.0m marks)
windows(height=3.5, width=8)
ggplot(meadow %>%
          filter(transect_location_m %in% c(0.5, 5.0))) +
   #
   geom_line(aes(x = distance, y = bg_biomass, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = bg_biomass), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



#- Macroalgae parameters viewed along transects -#

# MA density
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = total_ma_density, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = total_ma_density), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Macroalgal~density)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# MA density, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(total_ma_density, na.rm=T), se = se(total_ma_density), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "MA density") +
   theme_classic()


# Total MA biomass
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = total_ma_biomass, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = total_ma_biomass), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Macroalgal~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# MA biomass, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(total_ma_biomass, na.rm=T), se = se(total_ma_biomass), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "MA biomass") +
   theme_classic()



#- Burrow density viewed along transects -#

# small burrows
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = Burrow_sm_density, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = Burrow_sm_density), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Small~burrow~density~(no.~m^-2))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# total burrows
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = burrow_density, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = burrow_density), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Burrow~density~(no.~m^-2))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# burrow density, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(burrow_density, na.rm=T), se = se(burrow_density), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "Burrow density") +
   theme_classic()



