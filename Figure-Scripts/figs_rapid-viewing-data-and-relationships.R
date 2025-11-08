#
# Exploring relationships between variables
#



# Surface sulfide vs rhizome sulfide
windows(height=3.5, width=8)
ggplot(meadow) +
   geom_point(aes(x = rhizome_pwS, y = surface_pwS), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=8)
   ggplot(meadow) +
      geom_point(aes(x = log(rhizome_pwS), y = log(surface_pwS)), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))

   # pooled (by site)
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = rhizome_pwS, y = surface_pwS, color=site_id), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))

   # pooled (by treatment)
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = rhizome_pwS, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))

   # log, pooled (by treatment)
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = log(rhizome_pwS), y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))


   # log, pooled (by treatment) - switching axes (rhizome on y-axis)
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = log(surface_pwS), y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
      geom_smooth(aes(x = log(surface_pwS), y = log(rhizome_pwS), color = treatment), method="lm", se=F) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))




# Surface sulfide vs sediment OM
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = perc_om, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = perc_om, y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Rhizome sulfide vs sediment OM
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = perc_om, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = perc_om, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs sediment Porosity
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = porosity, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = porosity, y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Rhizome sulfide vs sediment Porosity
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = porosity, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = porosity, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs total burrow density
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = burrow_density, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = burrow_density, y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Rhizome sulfide vs total burrow density
windows(height=3.5, width=7)
ggplot(meadow) +
   geom_point(aes(x = burrow_density, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

   # log
   windows(height=3.5, width=7)
   ggplot(meadow) +
      geom_point(aes(x = burrow_density, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
      # scale_y_continuous(name = expression(Macroalgal~biomass)) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
      theme_classic() +
      theme(panel.border = element_rect(color="black", fill=NA))



# Sediment OM vs burrow density
windows(height=2.5, width=5)
ggplot(meadow) +
   geom_point(aes(x = burrow_density, y = perc_om, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Sediment DBD vs burrow density
windows(height=2.5, width=5)
ggplot(meadow) +
   geom_point(aes(x = burrow_density, y = dbd, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Sediment Porosity vs burrow density
windows(height=2.5, width=5)
ggplot(meadow) +
   geom_point(aes(x = burrow_density, y = porosity, color = treatment), size=3, alpha = 0.5) +
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs burrow density (trt/dist means)
ggplot(meadow %>%
          summarize(across(c(surface_pwS, burrow_density), ~mean(., na.rm=T)), .by = c(treatment, distance))) +
   geom_point(aes(x = burrow_density, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   theme_classic()
   
   # log Surface sulfide vs burrow density (trt/dist means)
   ggplot(meadow %>%
             mutate(surface_pwS = log(surface_pwS)) %>%
             summarize(across(c(surface_pwS, burrow_density), ~mean(., na.rm=T)), .by = c(treatment, distance))) +
      geom_point(aes(x = burrow_density, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      theme_classic()


# Rhizome sulfide vs burrow density (trt/dist means)
ggplot(meadow %>%
          summarize(across(c(rhizome_pwS, burrow_density), ~mean(., na.rm=T)), .by = c(treatment, distance))) +
   geom_point(aes(x = burrow_density, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
   theme_classic()

   # log Rhizome sulfide vs burrow density (trt/dist means)
   ggplot(meadow %>%
             mutate(rhizome_pwS = log(rhizome_pwS)) %>%
             summarize(across(c(rhizome_pwS, burrow_density), ~mean(., na.rm=T)), .by = c(treatment, distance))) +
      geom_point(aes(x = burrow_density, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
      scale_color_manual(values = c("vegetated" = "#41A67E", "unvegetated" = "#05339C")) +
      lims(x = c(0,60), y = c(3,5)) +
      theme_classic()
   



