#
# Evaluate relationship between variables and distance from the veg-unveg boundary
# 

library(car)
library(nlme)
library(AER)



# Is surface PW influenced by distance from the edge? 
AER::tobit(log(surface_pwS+0.001) ~ transect_location_m * treatment, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(surface_pwS+0.001) ~ transect_location_m * site_name, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(surface_pwS+0.001) ~ transect_location_m + site_name, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(surface_pwS+0.001) ~ transect_location_m, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(surface_pwS+0.001) ~ transect_location_m + treatment, left=log(0.001), data=meadow) %>% 
   emmeans(., ~treatment) %>%
   contrast(., method="pairwise")
## surface sulfide does not vary w/ distance from the edge
## p = 0.056; sulfide lower in vegetated areas; this result already in the 'stat_site-site-differences' script


# Is rhizome PW influenced by distance from the edge? 
AER::tobit(log(rhizome_pwS+0.001) ~ transect_location_m * treatment, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(rhizome_pwS+0.001) ~ transect_location_m * site_name, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(rhizome_pwS+0.001) ~ transect_location_m, left=log(0.001), data=meadow) %>% summary  # no

AER::tobit(log(rhizome_pwS+0.001) ~ transect_location_m * (site_id + treatment), left=log(0.001), data=meadow) %>% summary  # no
## rhizome sulfide does not vary w/ distance from the edge
## does not vary among sites/patches
## only sig. effect is treatment (veg/unveg); 
## p = 0.04; rhizome sulfide is lower in veg treatment; this result already in the 'stat_site-site-differences' script




#- Are sediment parameters related to distance from the edge? 

# DBD

# random intercept by patch
mod.bd.d = lme(dbd ~ treatment * transect_location_m, random = ~1|site_id, data = meadow)
# random intercept by site
mod.bd.d1 = lme(dbd ~ treatment * transect_location_m, random = ~1|site_name, data = meadow)
# without random effect
mod.bd.d2 = gls(dbd ~ treatment * transect_location_m, data = meadow)

# compare models
anova(mod.bd.d, mod.bd.d1, mod.bd.d2)
## first model, mod.bd.d, is best

broom.mixed::tidy(mod.bd.d)
## yes, DBD is related to distance, and sig. interaction for distance x treatment
## negative in Veg, positive in Unveg


# Porosity

# random intercept by patch
mod.p.d = lme(porosity ~ treatment * transect_location_m, random = ~1|site_id, data = meadow)
# random intercept by site
mod.p.d1 = lme(porosity ~ treatment * transect_location_m, random = ~1|site_name, data = meadow)
# without random effect
mod.p.d2 = gls(porosity ~ treatment * transect_location_m, data = meadow)

# compare models
anova(mod.p.d, mod.p.d1, mod.p.d2)
## first model, mod.p.d, is best

broom.mixed::tidy(mod.p.d)
## yes, porosity is related to distance, and sig. interaction for distance x treatment
## positive in Veg, negative in Unveg


# Organic Matter

# random intercept by patch
mod.om.d = lme(perc_om ~ treatment * transect_location_m, random = ~1|site_id, data = meadow)
# random intercept by site
mod.om.d1 = lme(perc_om ~ treatment * transect_location_m, random = ~1|site_name, data = meadow)
# without random effect
mod.om.d2 = gls(perc_om ~ treatment * transect_location_m, data = meadow)

# compare models
anova(mod.om.d, mod.om.d1, mod.om.d2)
## first model, mod.om.d, is best

broom.mixed::tidy(mod.om.d)
## no, effect of distance is nearly sig. (p=0.067), treatment is not sig (p=0.2), interaction is not sig. (p=0.11)





