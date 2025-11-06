#
# Exploring relationships between variables
#



# Surface sulfide vs rhizome sulfide
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_point(aes(x = rhizome_pwS, y = surface_pwS), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_point(aes(x = log(rhizome_pwS), y = log(surface_pwS)), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# pooled (by site)
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = rhizome_pwS, y = surface_pwS, color=site_id), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# pooled (by treatment)
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = rhizome_pwS, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log, pooled (by treatment)
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = log(rhizome_pwS), y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs sediment OM
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = perc_om, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = perc_om, y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Rhizome sulfide vs sediment OM
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = perc_om, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = perc_om, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs sediment Porosity
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = porosity, y = surface_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = porosity, y = log(surface_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Rhizome sulfide vs sediment Porosity
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = porosity, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = porosity, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome sulfide vs small burrow density
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = Burrow_sm_density, y = rhizome_pwS, color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

# log
windows(height=3.5, width=7)
ggplot(meadow) +
   #
   geom_point(aes(x = Burrow_sm_density, y = log(rhizome_pwS), color = treatment), size=3, alpha = 0.5) +
   #
   # scale_y_continuous(name = expression(Macroalgal~biomass)) +
   # facet_wrap(facets = vars(site_id), nrow=2, scales="free") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



