#
# Evaluate relationship between variables and distance from the veg-unveg boundary
# 

library(car)
library(nlme)
library(AER)


#- Is porewater sulfide related to distance from the seagrass edge? 

# Surface S

mod.s1 = lme(log(surface_pwS) ~ (site_name + treatment) * transect_location_m, 
             random= ~1|site_id, 
             meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(mod.s1))
summary(mod.s1)

mod.s2 = gls(log(surface_pwS) ~ (site_name + treatment) * transect_location_m, 
             meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(mod.s2))

anova(mod.s1, mod.s2)  # mod s1 with random effect is better

mod.s3 = lme(log(surface_pwS) ~ site_name + treatment + transect_location_m + site_name:transect_location_m + treatment:transect_location_m + site_name:treatment:transect_location_m,
             random= ~1|site_id, 
             meadow %>% filter(!(is.na(surface_pwS))))
summary(mod.s3)

mod.s4 = lme(log(surface_pwS) ~ site_name + treatment * transect_location_m,
             random= ~1|site_id, 
             meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(mod.s4))
summary(mod.s4)




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





