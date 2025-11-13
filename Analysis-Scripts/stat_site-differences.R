#
# Test for differences between sites and treatments
#

library(car)
library(nlme)
library(emmeans)
# library(AER)
# library(glmmTMB)
library(ggResidpanel)

set.seed(123)


#- Surface porewater S -# 

# is surface pwS sig. diff. between sites? 
aov(log(surface_pwS) ~ site_name, meadow) %>% summary  # no; p = 0.29

pws.a1 = aov(surface_pwS ~ site_name, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.a1))  # non-normal

pws.a2 = aov(log(surface_pwS) ~ site_name, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.a2))  # good

pws.l1 = lm(surface_pwS ~ site_name, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.l1))

pws.l2 = lm(log(surface_pwS) ~ site_name, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.l2))

# lm and aov produce same results


# trying an LME
pws.m1 = lme(surface_pwS ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m1))  # not great

pws.m2 = lme(log(surface_pwS) ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m2))  # better, not as smooth as lm or aov

# gls, remove random effect
pws.m3 = gls(log(surface_pwS) ~ site_name, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m3))

anova(pws.m3, pws.m2)  # m2, lme() with random effects is better


# is surface pwS sig. diff. between treatments? 
t.test(log(surface_pwS) ~ treatment, meadow)  # no, but close; p = 0.067

pws.l3 = lm(surface_pwS ~ treatment, meadow)
qqnorm(residuals(pws.l3))

pws.l4 = lm(log(surface_pwS) ~ treatment, meadow)
qqnorm(residuals(pws.l4))


# trying an LME

# treatment and site together
pws.m4 = lme(surface_pwS ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m4))
resid_panel(pws.m4)

pws.m5 = lme(log(surface_pwS) ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))), method="ML")
qqnorm(residuals(pws.m5))
resid_panel(pws.m5)  # very good fit; m5 is a good model for surface porewater sulfide
summary(pws.m5)

# with an interaction term
pws.m6 = lme(log(surface_pwS) ~ treatment * site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))), method="ML")
qqnorm(residuals(pws.m6))
resid_panel(pws.m6)  # very good fit

anova(pws.m6, pws.m5)  # not significant; simpler m5, without interaction, is the best model for surface sulfide

# contrasts
emmeans(update(pws.m5, method="REML"), ~treatment|site_name)
contrast(emmeans(update(pws.m5, method="REML"), ~treatment), method="pairwise")
## difference in surface PW sulfide between Veg and Unveg treatments is not sig (but p = 0.09)
contrast(emmeans(update(pws.m5, method="REML"), ~site_name), method="pairwise")
## no differences between sites



#- Rhizome porewater S -# 

# is rhizome pwS sig. diff. between sites? 
aov(log(rhizome_pwS) ~ site_name, meadow) %>% summary  # no; p = 0.7

pwr.a1 = aov(rhizome_pwS ~ site_name, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.a1))

pwr.a2 = aov(log(rhizome_pwS) ~ site_name, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.a2))

# trying lme
pwr.m1 = lme(rhizome_pwS ~ site_name, random = ~1|site_id, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.m1))

pwr.m2 = lme(log(rhizome_pwS) ~ site_name, random = ~1|site_id, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.m2))

pwr.m3 = gls(log(rhizome_pwS) ~ site_name, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.m3))

anova(pwr.m2, pwr.m3)  # not sig., simpler gls model without random effects is better


# is rhizome pwS diff. between treatments? 
t.test(log(rhizome_pwS) ~ treatment, meadow)  # no; p = 0.12

# trying an lme

# site and treatment together
pwr.m4 = lme(rhizome_pwS ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(rhizome_pwS))))
qqnorm(residuals(pwr.m4))

pwr.m5 = lme(log(rhizome_pwS) ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(rhizome_pwS))), method="ML")
qqnorm(residuals(pwr.m5))

# with interaction
pwr.m6 = lme(log(rhizome_pwS) ~ treatment * site_name, random = ~1|site_id, meadow %>% filter(!(is.na(rhizome_pwS))), method="ML")
qqnorm(residuals(pwr.m6))

anova(pwr.m5, pwr.m6)  # not sig., simpler model m5 without interaction is better


# contrasts
emmeans(update(pwr.m5, method="REML"), ~treatment|site_name)
contrast(emmeans(update(pwr.m5, method="REML"), ~treatment), method="pairwise")
## difference in rhizome PW sulfide between Veg and Unveg treatments is not sig (p = 0.24)
contrast(emmeans(update(pwr.m5, method="REML"), ~site_name), method="pairwise")
## no differences between sites




#- Dry Bulk Density -# 

# between sites? 

# lme() using random effect for patch
mod.bd = lme(dbd ~ site_name * treatment, random = ~1|site_id, data = meadow)
summary(mod.bd)
Anova(mod.bd, type=3)
qqnorm(residuals(mod.bd))  # possible outlier values at the low end

emmeans(mod.bd, ~treatment|site_name)
contrast(emmeans(mod.bd, ~treatment|site_name),
         method = "pairwise")
## DBD is sig. greater in Unveg patches at all sites

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
qqnorm(residuals(mod.om))  # possible outlier values at the high end

emmeans(mod.om, ~treatment|site_name)
contrast(emmeans(mod.om, ~treatment|site_name),
         method = "pairwise")
## OM is sig. greater in Veg areas at Craig and Conch, no diff. between treatments at Anne's

contrast(emmeans(mod.om, ~treatment),
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



#- Porosity -# 

# between sites? 

# lme() using random effect for patch
mod.por = lme(porosity ~ site_name * treatment, random = ~1|site_id, data = meadow)
summary(mod.por)
Anova(mod.por, type=3)
qqnorm(residuals(mod.por))  # possible outlier values at the low end

emmeans(mod.por, ~treatment|site_name)
contrast(emmeans(mod.por, ~treatment|site_name),
         method = "pairwise")
## porosity is sig. greater in Veg patches at all sites



#- Burrow Density -#

# between sites? 

# lme() using random effect for patch
mod.bur = lme(burrow_density ~ site_name * treatment, random = ~1|site_id, data = meadow)
summary(mod.bur)
Anova(mod.bur, type=3)
qqnorm(residuals(mod.bur))  # possible outlier values at the low end

emmeans(mod.bur, ~treatment|site_name)
contrast(emmeans(mod.bur, ~treatment|site_name),
         method = "pairwise")
## burrows significantly greater in Unveg areas at Craig and Little Conch
## burrows also higher in Unveg at Anne's, but diff. isn't sig. (p=0.42)



