#
# Test for differences between sites and treatments
#

library(car)
library(nlme)
library(emmeans)
library(AER)
library(glmmTMB)
library(ggResidpanel)


#- Surface porewater S -# 

# is surface pwS sig. diff. between sites? 
aov(log(surface_pwS) ~ site_name, meadow) %>% summary  # no; p = 0.29

pws.a1 = aov(surface_pwS ~ site_name, meadow)
qqnorm(residuals(pws.a1))

pws.a2 = aov(log(surface_pwS) ~ site_name, meadow)
qqnorm(residuals(pws.a2))

pws.l1 = lm(surface_pwS ~ site_name, meadow)
qqnorm(residuals(pws.l1))

pws.l2 = lm(log(surface_pwS) ~ site_name, meadow)
qqnorm(residuals(pws.l2))


# trying an LME
pws.m1 = lme(surface_pwS ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m1))

pws.m2 = lme(log(surface_pwS) ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m2))



# is surface pwS sig. diff. between treatments? 
t.test(log(surface_pwS) ~ treatment, meadow)  # no, but close; p = 0.067

pws.l3 = lm(surface_pwS ~ treatment, meadow)
qqnorm(residuals(pws.l3))

pws.l4 = lm(log(surface_pwS) ~ treatment, meadow)
qqnorm(residuals(pws.l4))


# trying an LME
pws.m3 = lme(surface_pwS ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m3))
resid_panel(pws.m3)

pws.m4 = lme(log(surface_pwS) ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m4))
resid_panel(pws.m4)




#- Rhizome porewater S -# 

# is rhizome pwS sig. diff. between sites? 
aov(log(rhizome_pwS) ~ site_name, meadow) %>% summary  # no; p = 0.7


# is rhizome pwS diff. between treatments? 
t.test(log(rhizome_pwS) ~ treatment, meadow)  # no; p = 0.12




#- Dry Bulk Density -# 

# between sites? 

# lme() using random effect for patch
mod.bd = lme(dbd ~ site_name * treatment, random = ~1|site_id, data = meadow)
summary(mod.bd)
Anova(mod.bd, type=3)
qqnorm(residuals(mod.bd))

emmeans(mod.bd, ~treatment|site_name)
contrast(emmeans(mod.bd, ~treatment|site_name),
         method = "pairwise")

# standard anova
mod.bd1 = aov(dbd ~ treatment * site_name, data = meadow)
summary(mod.bd1)
Anova(mod.bd1, type=3)
qqnorm(residuals(mod.bd1))

emmeans(mod.bd1, ~treatment|site_name)
contrast(emmeans(mod.bd1, ~treatment|site_name),
         method = "pairwise")


## values are a bit different between lme and aov, but conclusions are the same
## sig. effect of treatment, DBD sig. higher in unveg. treatment at all sites



#- Organic Matter -# 
mod.om = lme(perc_om ~ site_name * treatment, random = ~1|site_id, data = meadow)
summary(mod.om)
Anova(mod.om, type=3)
qqnorm(residuals(mod.om))

emmeans(mod.om, ~treatment|site_name)
contrast(emmeans(mod.om, ~treatment|site_name),
         method = "pairwise")

# gls (no random effect)
mod.om2 = gls(perc_om ~ site_name * treatment, data = meadow)
Anova(mod.om2, type=3)
qqnorm(residuals(mod.om2))

anova(mod.om, mod.om2)
# lme model with random effect is better

# standard anova
mod.om1 = aov(perc_om ~ site_name * treatment, data = meadow)
Anova(mod.om1, type=3)
qqnorm(residuals(mod.om1))

emmeans(mod.om1, ~treatment|site_name)
contrast(emmeans(mod.om1, ~treatment|site_name),
         method = "pairwise")


## values are a bit different between lme, gls, and aov, but conclusions are the same
## OM sig. diff. between V-U at Craig and Little Conch (and OM is higher in veg. areas), but not at Anne's
## model says site and treatment are both sig., but the interaction is not sig., so not sure why contrast() shows diff results between sites...







