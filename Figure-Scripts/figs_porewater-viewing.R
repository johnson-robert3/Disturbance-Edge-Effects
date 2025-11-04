#
# Figures for viewing porewater sulfide data
#

library(tidyverse)


# run 'data_spatial-porewater-S' and 'data_meadow-data' scripts first to generate data


# add a new variable for graphing distance along the transect
meadow = meadow %>%
   mutate(distance = if_else(treatment=="unvegetated", transect_location_m * -1, transect_location_m))



#-- Surface Porewater S viewed along transects --#

# Surface PW S based on distance from edge, all sites and treatments together
windows(height=3.5, width=5)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = distance, y = surface_pwS, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   

# Surface PW S based on distance from edge, by site
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   # geom_smooth(aes(x = distance, y = surface_pwS, group = treatment), color="black", linewidth=1) +
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surface_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_wrap(facets = vars(site_name)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Surface PW S based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surface_pwS), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   


#-- Rhizome Porewater S viewed along transects --#

# Rhizome porewater based on distance from edge, by site
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhizome_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_name)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome PW S based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhizome_pwS), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome porewater based on distance from edge, but treatments separated (by patch and treatment)
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   # geom_smooth(aes(x = transect_location_m, y = rhizome_pwS, group = treatment), color="black", linewidth=1) +
   geom_line(aes(x = transect_location_m, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   geom_hline(aes(yintercept=0), linetype=3, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_grid(rows = vars(treatment),
              cols = vars(site_id)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Surface and Rhizome porewater, all patches and separated by treatment
windows(height=3.5, width=8)
ggplot(meadow) +
   # rhizome
   geom_line(aes(x = transect_location_m, y = rhizome_pwS), 
             linetype = 1, linewidth= 0.75, alpha=0.5, color="seagreen4") +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS), size=2, alpha = 0.5, shape=17, color="seagreen4") +
   # surface
   geom_line(aes(x = transect_location_m, y = surface_pwS), 
             linetype = 1, linewidth= 0.75, alpha=0.5, color="cornflowerblue") +
   geom_point(aes(x = transect_location_m, y = surface_pwS), size=2, alpha = 0.5, shape=22, color="cornflowerblue") +
   # 
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   geom_hline(aes(yintercept=0), linetype=3, color="gray50") +
   #
   scale_y_continuous(name = expression(Sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_grid(rows = vars(treatment),
              cols = vars(site_id)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



#-- Porewater S, unveg relative to veg --#

# create temporary dataset for difference in sulfide between Veg and Unveg treatments
tmp = meadow %>%
   arrange(site_id, transect_location_m, desc(treatment)) %>% 
   select(site_id, transect_location_m, treatment, contains("pwS")) %>%
   # calculate the difference between veg and unveg for corresponding distances on the transect for each patch
   # positive value means sulfide is higher in the seagrass
   # negative value means suflide is higher in the bare patch
   summarize(surf_s_diff = surface_pwS[treatment=="vegetated"] - surface_pwS[treatment=="unvegetated"], 
             rhiz_s_diff = rhizome_pwS[treatment=="vegetated"] - rhizome_pwS[treatment=="unvegetated"], 
             .by = c(site_id, transect_location_m))


# Surface S, view variation in the data across transect distances
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = surf_s_diff), position = position_jitter(height=0, width=0.07))
## potential outlier point(s) at 3.0m??

# Rhizome S, view variation in the data across transect distances
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = rhiz_s_diff), position = position_jitter(height=0, width=0.07))
## potential outlier point at 3.0m??




