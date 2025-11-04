#
# Evaluate relationship between variables and distance from the veg-unveg boundary
# 

library(car)
library(nlme)
library(AER)



# Is surface PW influenced by distance from the edge? 


# Is rhizome PW influenced by distance from the edge? 




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





