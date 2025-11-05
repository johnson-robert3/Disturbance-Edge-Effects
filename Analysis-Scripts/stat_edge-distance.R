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




# Is porewater significantly different on either side of the vegetated boundary? 

# create temporary dataset for difference in sulfide between Veg and Unveg treatments
tmp = meadow %>%
   arrange(site_id, transect_location_m, desc(treatment)) %>% 
   select(site_id, transect_location_m, treatment, contains("pwS")) %>%
   # calculate the difference between veg and unveg for corresponding distances on the transect for each patch
   #- positive value means sulfide is higher in the seagrass
   #- negative value means suflide is higher in the bare patch
   summarize(surf_s_diff = surface_pwS[treatment=="vegetated"] - surface_pwS[treatment=="unvegetated"], 
             rhiz_s_diff = rhizome_pwS[treatment=="vegetated"] - rhizome_pwS[treatment=="unvegetated"], 
             .by = c(site_id, transect_location_m))

# is the V-U difference in surface sulfide at 0.5m sig. diff. from 0? (suggestive of a trend toward higher S in one treatment)
t.test(tmp %>% filter(transect_location_m==0.5) %>% pull(surf_s_diff), mu=0)  # no, p = 0.83
t.test(tmp %>% filter(transect_location_m==1.0) %>% pull(surf_s_diff), mu=0)  # no, p = 0.37
t.test(tmp %>% filter(transect_location_m==2.0) %>% pull(surf_s_diff), mu=0)  # no, p = 0.85
t.test(tmp %>% filter(transect_location_m==3.0) %>% pull(surf_s_diff), mu=0)  # no, p = 0.26
t.test(tmp %>% filter(transect_location_m==4.0) %>% pull(surf_s_diff), mu=0)  # no, p = 0.90
t.test(tmp %>% filter(transect_location_m==5.0) %>% pull(surf_s_diff), mu=0)  # no, p = 0.10


# is the V-U difference in rhizome sulfide at 0.5m sig. diff. from 0? (suggestive of a trend toward higher S in one treatment)
t.test(tmp %>% filter(transect_location_m==0.5) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.1
t.test(tmp %>% filter(transect_location_m==1.0) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.62
t.test(tmp %>% filter(transect_location_m==2.0) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.26
t.test(tmp %>% filter(transect_location_m==3.0) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.78
t.test(tmp %>% filter(transect_location_m==4.0) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.81
t.test(tmp %>% filter(transect_location_m==5.0) %>% pull(rhiz_s_diff), mu=0)  # no, p = 0.61


# is surface sulfide near the edge (pooling 0.5 - 2m) sig. diff. between seagrass and bare patches at the patch level?
meadow %>%
   select(site_id, transect_location_m, treatment, surface_pwS) %>%
   filter(transect_location_m <= 2) %>%
   pivot_wider(id_cols = c(site_id, transect_location_m),
               names_from = "treatment",
               values_from = "surface_pwS") %>%
   arrange(site_id) %>%
   group_by(site_id) %>%
   group_map(.f = ~t.test(.x$vegetated, .x$unvegetated, paired=TRUE))
## surface sulfide is not sig. diff. between V and U near the edge in any patch


# is surface sulfide near the middle (pooling 3 - 5m) sig. diff. between seagrass and bare patches at the patch level?
meadow %>%
   select(site_id, transect_location_m, treatment, surface_pwS) %>%
   filter(transect_location_m >= 3) %>%
   pivot_wider(id_cols = c(site_id, transect_location_m),
               names_from = "treatment",
               values_from = "surface_pwS") %>%
   arrange(site_id) %>%
   # remove S2.3, b/c only one veg. sample for sulfide
   filter(!site_id=="S2.3") %>%
   group_by(site_id) %>%
   group_map(.f = ~t.test(.x$vegetated, .x$unvegetated, paired=TRUE, na.rm=TRUE))
## surface sulfide is not sig. diff. between V and U near the middle in any patch



# is rhizome sulfide near the edge (pooling 0.5 - 2m) sig. diff. between seagrass and bare patches at the patch level?
meadow %>%
   select(site_id, transect_location_m, treatment, rhizome_pwS) %>%
   filter(transect_location_m <= 2) %>%
   pivot_wider(id_cols = c(site_id, transect_location_m),
               names_from = "treatment",
               values_from = "rhizome_pwS") %>%
   arrange(site_id) %>%
   # drop sites 1.3 and 2.3, b/c only one sulfide sample to compare
   filter(!(site_id %in% c("S1.3", "S2.3"))) %>%
   group_by(site_id) %>%
   group_map(.f = ~t.test(.x$vegetated, .x$unvegetated, paired=TRUE))
## rhizome sulfide is not sig. diff. between V and U near the edge in any patch


# is rhizome sulfide near the middle (pooling 3 - 5m) sig. diff. between seagrass and bare patches at the patch level?
meadow %>%
   select(site_id, transect_location_m, treatment, rhizome_pwS) %>%
   filter(transect_location_m >= 3) %>%
   pivot_wider(id_cols = c(site_id, transect_location_m),
               names_from = "treatment",
               values_from = "rhizome_pwS") %>%
   arrange(site_id) %>%
   # drop sites 1.1, b/c only one sulfide sample to compare
   filter(!(site_id=="S1.1")) %>%
   group_by(site_id) %>%
   group_map(.f = ~t.test(.x$vegetated, .x$unvegetated, paired=TRUE))
## rhizome sulfide is not sig. diff. between V and U near the middle in any patch





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





