
# just getting some values and summary statistics





# coefficient of variation
meadow %>%
   mutate(logsurf = log(surface_pwS), logrhiz = log(rhizome_pwS)) %>%
   summarize(across(c(surface_pwS, rhizome_pwS, logsurf, logrhiz), ~ sd(., na.rm=T)/mean(., na.rm=T)), .by=c(site_name, treatment))



# range

# by treatment
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(min = ~min(.x, na.rm=T), max = ~max(.x, na.rm=T))), 
             .by = treatment)
# by site
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(min = ~min(.x, na.rm=T), max = ~max(.x, na.rm=T))), 
             .by = site_name)

# by patch
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(min = ~min(.x, na.rm=T), max = ~max(.x, na.rm=T))), 
             .by = site_id)

# site and treatment
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density), 
                    list(min = ~min(.x, na.rm=T), max = ~max(.x, na.rm=T))), 
             .by = c(site_name, treatment))


# mean and median
# by treatment
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(mean = ~mean(.x, na.rm=T), median = ~median(.x, na.rm=T))), 
             .by = treatment)

# by site
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(mean = ~mean(.x, na.rm=T), median = ~median(.x, na.rm=T))), 
             .by = site_name)

# by patch
meadow %>%
   summarize(across(c(surface_pwS, rhizome_pwS, dbd, porosity, perc_om, burrow_density, Tt_biomass, Tt_density, lai), 
                    list(mean = ~mean(.x, na.rm=T), median = ~median(.x, na.rm=T))), 
             .by = site_id)



# Bulk density
meadow %>%
   summarize(mean = mean(dbd, na.rm=T), se = se(dbd), .by=c(site_name, treatment))

meadow %>%
   summarize(mean = mean(dbd, na.rm=T), se = se(dbd), .by=site_name)

meadow %>%
   summarize(mean = mean(dbd, na.rm=T), se = se(dbd), .by=treatment)


# OM
meadow %>%
   summarize(mean = mean(perc_om, na.rm=T), se = se(perc_om), .by=c(site_name, treatment))

meadow %>%
   summarize(mean = mean(perc_om, na.rm=T), se = se(perc_om), .by=site_name)

meadow %>%
   rename(tmp = perc_om) %>%
   summarize(min = min(tmp, na.rm=T),
             max = max(tmp, na.rm=T), 
             mean = mean(tmp, na.rm=T), 
             se = se(tmp),
             cv = sd(tmp, na.rm=T)/mean(tmp, na.rm=T), .by=treatment)


# Porosity
meadow %>%
   summarize(mean = mean(porosity, na.rm=T), se = se(porosity), .by=c(site_name, treatment))

meadow %>%
   summarize(mean = mean(porosity, na.rm=T), se = se(porosity), .by=site_name)

meadow %>%
   rename(tmp = porosity) %>%
   summarize(min = min(tmp, na.rm=T),
             max = max(tmp, na.rm=T), 
             mean = mean(tmp, na.rm=T), 
             se = se(tmp),
             cv = sd(tmp, na.rm=T)/mean(tmp, na.rm=T), .by=treatment)


# burrow density
meadow %>%
   summarize(mean = mean(burrow_density, na.rm=T), se = se(burrow_density), .by=c(site_name, treatment))

meadow %>%
   summarize(mean = mean(burrow_density, na.rm=T), se = se(burrow_density), .by=site_name)

meadow %>%
   rename(tmp = burrow_density) %>%
   summarize(min = min(tmp, na.rm=T),
             max = max(tmp, na.rm=T), 
             mean = mean(tmp, na.rm=T), 
             se = se(tmp),
             cv = sd(tmp, na.rm=T)/mean(tmp, na.rm=T), .by=treatment)





