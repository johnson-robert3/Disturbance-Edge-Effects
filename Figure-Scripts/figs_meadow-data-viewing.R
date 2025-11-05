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






