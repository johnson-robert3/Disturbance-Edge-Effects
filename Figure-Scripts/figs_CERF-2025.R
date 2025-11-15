#~~~
# Figures for CERF 2025 presentation
#
# Spatial sediments for vegetated/unvegetated patches
#
# By: R. Johnson
#~~~


# run 'data_spatial-porewater-S' and 'data_meadow-data' scripts to create data sets


# set new working directory for outputting figures
setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance - for Robert")


#- figure aesthetics
vu_fill = c("vegetated" = "#59C1BD", "unvegetated" = "#0D4C92")
vu_break = c("unvegetated", "vegetated") 
vu_labs = c("Unveg", "Veg")


se = function(.x) { sd(.x, na.rm=T) / sqrt(length(.x)) }


# add a new variable for graphing distance along the transect
meadow = meadow %>%
   mutate(distance = if_else(treatment=="unvegetated", transect_location_m * -1, transect_location_m))



#--
# Porewater Sulfide
#-- 

#- Surface sulfide along transect (mean+SE)
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(surface_pwS, na.rm=T), se = se(surface_pwS), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Sulfide[(surface)]~(mu*M))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_surface-sulfide.png")


#- Rhizome sulfide along transect (mean+SE)
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(rhizome_pwS, na.rm=T), se = se(rhizome_pwS), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Sulfide[(rhizome)]~(mu*M))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_rhizome-sulfide.png")


#- Relationship between rhizome sulfide and surface sulfide (log transformed data)
windows(height=3, width=4)
ggplot(meadow) +
   geom_smooth(aes(x = log(surface_pwS), y = log(rhizome_pwS)), color="black", method="lm", se=F) +
   geom_point(aes(x = log(surface_pwS), y = log(rhizome_pwS), fill = treatment), shape=21, size=3, alpha = 0.8) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   labs(x = expression(ln~(Sulfide[(surface)])~(mu*M)),
        y = expression(ln~(Sulfide[(rhizome)])~(mu*M))) +
   theme_classic() +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/rhizome-vs-surface-sulfide.png")



#--
# Sediments
#-- 

#- Dry Bulk Density
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(dbd, na.rm=T), se = se(dbd), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Dry~bulk~density~(g~cm^-3))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_DBD.png")


#- Porosity
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(porosity, na.rm=T), se = se(porosity), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Sediment~porosity)) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_porosity.png")


#- Percent OM
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(perc_om, na.rm=T), se = se(perc_om), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Organic~matter~("%"))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_percent-OM.png")


#- Burrow density (total burrows)
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(burrow_density, na.rm=T), se = se(burrow_density), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Burrow~density~(num.~m^-2))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_burrow-density.png")



#--
# Seagrass parameters
#--

#- Total seagrass biomass
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(total_sg_biomass, na.rm=T), se = se(total_sg_biomass), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Seagrass~biomass~(g~DM~m^-2))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1)))

ggsave(file = "Figures/CERF 2025/transect_seagrass-biomass.png")


#- Thalassia density
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(Tt_density, na.rm=T), se = se(Tt_density), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(italic(Thalassia)~density~(shoots~m^-2))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_Tt-density.png")


#- Thalassia Leaf Area Index
windows(height=3, width=4)
ggplot(meadow %>%
          mutate(lai = if_else(lai=="NaN", 0, lai)) %>%
          summarize(mean = mean(lai, na.rm=T), se = se(lai), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Leaf~area~index)) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_LAI.png")


#- Rhizome biomass (only at the 0.5m and 5.0m)
windows(height=3, width=4)
ggplot(meadow %>%
          filter(transect_location_m %in% c(0.5, 5.0)) %>%
          summarize(mean = mean(bg_biomass, na.rm=T), se = se(bg_biomass), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Belowground~biomass~(g~m^-2))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_rhizome-biomass.png")


#- Total macroalgal density
windows(height=3, width=4)
ggplot(meadow %>%
          summarize(mean = mean(total_ma_density, na.rm=T), se = se(total_ma_density), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0, color="gray40", linewidth=1) +
   geom_line(aes(x = distance, y = mean), color="gray60", linewidth=1) +
   geom_point(aes(x = distance, y = mean, fill = treatment), shape=21, size=5) +
   geom_vline(xintercept = 0, linetype=2) +
   scale_y_continuous(name = expression(Macroalgal~density~(thalli~m^-2))) +
   scale_x_continuous(name = expression(Distance~from~seagrass~edge~(m)), limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
   scale_fill_manual(breaks = vu_break, 
                     values = vu_fill) +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA),
         axis.text.x = element_text(color="black", size=rel(1.2)),
         axis.text.y = element_text(color="black", size=rel(1)),
         axis.title.y = element_text(color="black", size=rel(1), margin = margin(r=0.5, unit="line")),
         legend.position = "none",
         legend.text = element_text(size=rel(1.4)))

ggsave(file = "Figures/CERF 2025/transect_MA-density.png")


