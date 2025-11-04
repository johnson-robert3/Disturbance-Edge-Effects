#
# View data and evaluate need for data transformation
#


#- Surface porewater S

# view data
hist(meadow$surface_pwS)
ggplot(meadow) + geom_histogram(aes(surface_pwS)) # surface pw data need to be log transformed
ggplot(meadow) + geom_histogram(aes(log(surface_pwS+0.001))) # data are zero-inflated due to instrument detection limit

# view data by site
ggplot(meadow) +
   geom_histogram(aes(x = surface_pwS, fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # yes, all sites need to be log transformed

ggplot(meadow) +
   geom_histogram(aes(x = log(surface_pwS+0.001), fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # data are zero-inflated (especially Craig) due to instrument detection limit

# view data by treatment
ggplot(meadow) +
   geom_histogram(aes(x = surface_pwS, fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # yes, all treatments need to be log transformed

ggplot(meadow) +
   geom_histogram(aes(x = log(surface_pwS+0.001), fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # data are zero-inflated (especially vegetated) due to instrument detection limit

# rank plot
ggplot(meadow %>%
          arrange(surface_pwS) %>%
          mutate(rank = seq(length(.$surface_pwS)))) +
   geom_point(aes(x = rank, y = surface_pwS, color=treatment))


# Surface sulfide vs distance from the veg edge
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = surface_pwS, color = treatment), position = position_jitter(height=0, width=0.07))

ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(surface_pwS+0.001), color = treatment), position = position_jitter(height=0, width=0.07))



#- Rhizome porewater S

# view data
hist(meadow$rhizome_pwS)
ggplot(meadow) + geom_histogram(aes(rhizome_pwS)) # rhizome pw data need to be log transformed
ggplot(meadow) + geom_histogram(aes(log(rhizome_pwS+0.001))) # data are zero-inflated due to instrument detection limit

# view data by site
ggplot(meadow) +
   geom_histogram(aes(x = rhizome_pwS, fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # yes, all sites need to be log transformed

ggplot(meadow) +
   geom_histogram(aes(x = log(rhizome_pwS+0.001), fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # data are zero-inflated due to instrument detection limit

# view data by treatment
ggplot(meadow) +
   geom_histogram(aes(x = rhizome_pwS, fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # yes, all treatments need to be log transformed

ggplot(meadow) +
   geom_histogram(aes(x = log(rhizome_pwS+0.001), fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # data are zero-inflated (especially vegetated) due to instrument detection limit

# rank plot
ggplot(meadow %>%
          arrange(rhizome_pwS) %>%
          mutate(rank = seq(length(.$rhizome_pwS)))) +
   geom_point(aes(x = rank, y = rhizome_pwS, color=treatment))


# Rhizome sulfide vs distance from the veg edge
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS, color = treatment), position = position_jitter(height=0, width=0.07))

ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(rhizome_pwS+0.001), color = treatment), position = position_jitter(height=0, width=0.07))



#- Sediment DBD

# view data
hist(meadow$dbd)
ggplot(meadow) + geom_histogram(aes(dbd))
ggplot(meadow) + geom_histogram(aes(log(dbd))) # no, DBD data do not need to be transformed


# view data by site
ggplot(meadow) +
   geom_histogram(aes(x = dbd, fill = site_name)) +
   facet_wrap(facets = vars(site_name))

ggplot(meadow) +
   geom_histogram(aes(x = log(dbd), fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # log-transforming does not improve any sites

# outliers by site?
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = dbd))  # potentially outlier points (low) at Anne's and Craig sites

# view data by treatment
ggplot(meadow) +
   geom_histogram(aes(x = dbd, fill = treatment)) +
   facet_wrap(facets = vars(treatment))

ggplot(meadow) +
   geom_histogram(aes(x = log(dbd), fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # log-transforming does not improve either treatment

# outliers by treatment?
ggplot(meadow) +
   geom_boxplot(aes(x = treatment, y = dbd))  # potentially 3 outlier points (low) in vegetated treatment


# DBD vs distance from the veg edge
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = dbd, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = dbd, color = treatment), method="lm", se=F)


#- Sediment Porosity

# view data
hist(meadow$porosity)
ggplot(meadow) + geom_histogram(aes(porosity))
ggplot(meadow) + geom_histogram(aes(log(porosity))) # no, DBD data do not need to be transformed

# outliers?
ggplot(meadow) + geom_boxplot(aes(y=porosity))  # one potential outlier (high)

# outliers by treatment?
ggplot(meadow) +
   geom_boxplot(aes(x = treatment, y = porosity))  # no outliers when grouped by treatment


# Porosity vs distance from the veg edge
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = porosity, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = porosity, color = treatment), method="lm", se=F)



#- Sediment Organic Matter

# view data
hist(meadow$perc_om)
ggplot(meadow) + geom_histogram(aes(perc_om))
ggplot(meadow) + geom_histogram(aes(log(perc_om))) # no, OM data do not need to be transformed


# view data by site
ggplot(meadow) +
   geom_histogram(aes(x = perc_om, fill = site_name)) +
   facet_wrap(facets = vars(site_name))

ggplot(meadow) +
   geom_histogram(aes(x = log(perc_om), fill = site_name)) +
   facet_wrap(facets = vars(site_name)) # no sites need to be transformed

# outliers by site?
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = perc_om))  # potential outlier point (high) at  Craig site

# view data by treatment
ggplot(meadow) +
   geom_histogram(aes(x = perc_om, fill = treatment)) +
   facet_wrap(facets = vars(treatment))

ggplot(meadow) +
   geom_histogram(aes(x = log(perc_om), fill = treatment)) +
   facet_wrap(facets = vars(treatment)) # no treatments need to be transformed

# outliers by treatment?
ggplot(meadow) +
   geom_boxplot(aes(x = treatment, y = perc_om))  # potentially outlier points in unveg trt (high and low) and veg trt (high)


# OM vs distance from the veg edge
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = perc_om, color = treatment), position = position_jitter(height=0, width=0.07)) +
   geom_smooth(aes(x = transect_location_m, y = perc_om, color = treatment), method="lm", se=F)


# OM by site and treatment
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = perc_om, group=interaction(site_name, treatment), color = treatment))

#- Tt density

# view data
hist(meadow$Tt_density)
ggplot(meadow) + geom_histogram(aes(Tt_density)) # data are skewed (due to the lack of Tt in unvegetated patches)
ggplot(meadow) + geom_histogram(aes(log(Tt_density+1))) # 

## seagrass data should probably only be used to investigate relationships in vegetated areas
## by nature, seagrass are absent (or near absent) in unveg areas, so seagrass shouldn't be a strong predictor of biogeochem in unveg areas

# Tt density by treatment
ggplot(meadow) +
   geom_boxplot(aes(x = treatment, y = Tt_density))

# Tt density by site
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = Tt_density))

# Tt density by site and treatment
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = Tt_density, group=interaction(site_name, treatment), color = treatment))


#- Tt biomass
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = Tt_biomass, group=interaction(site_name, treatment), color = treatment))


#- Tt LAI
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = lai, group=interaction(site_name, treatment), color = treatment))


#- Macroalgal density
ggplot(meadow) +
   geom_boxplot(aes(x = site_name, y = total_ma_biomass, group=interaction(site_name, treatment), color = treatment))






