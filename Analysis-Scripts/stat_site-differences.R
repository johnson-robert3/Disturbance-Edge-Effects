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
aov(log(surface_pwS+0.001) ~ site_name, meadow) %>% summary  # no; p = 0.29

pws.a1 = aov(surface_pwS ~ site_name, meadow)
qqnorm(residuals(pws.a1))

pws.a2 = aov(log(surface_pwS+0.001) ~ site_name, meadow)
qqnorm(residuals(pws.a2))

pws.l1 = lm(surface_pwS ~ site_name, meadow)
qqnorm(residuals(pws.l1))

pws.l2 = lm(log(surface_pwS+0.001) ~ site_name, meadow)
qqnorm(residuals(pws.l2))


# trying an LME
pws.m1 = lme(surface_pwS ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m1))

pws.m2 = lme(log(surface_pwS+0.001) ~ site_name, random = ~1|site_id, data = meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m2))


# none of these model types can correctly handle the zeros in the data
# zeros represent when sulfide was seemingly absent from a sample (concentration = 0; no sulfide present)

#---
# try a two-step hurdle model
## first step is to evaluate relationships based on presence (what determines if sulfide is there?)
## second step is to evaluate relationships with variables when sulfide is present

# add a binary variable for sulfide presence/absence
meadow_hurdle = meadow %>%
   mutate(surf_pws_presence = if_else(surface_pwS==0, 0, 1), 
          rhiz_pws_presence = if_else(rhizome_pwS==0, 0, 1))


# test if site has an effect on whether sulfide is absent
h1 = glm(surf_pws_presence ~ site_name, data = meadow_hurdle %>% filter(!(is.na(surface_pwS))), family = "binomial")
summary(h1)  # no, no difference among sites for presence/absence

# test if the positive values of surface PW S differ among sites
h2 = lme(log(surface_pwS+0.001) ~ site_name, random = ~1|site_id,
         data = meadow_hurdle %>% filter(!(is.na(surface_pwS))) %>% filter(!(surf_pws_presence==0)))
summary(h2)  # also no difference among sites for positive values of surface PW S


# try a two-step hurdle model all at once using the glmmTMB::glmmTMB() function

# using raw data and a Gamma distribution with log-link
h3 = glmmTMB(surface_pwS ~ site_name, 
             ziformula = ~site_name,
             family = Gamma(link = "log"),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h3)

# using log-transformed data and a gaussian distribution
h4 = glmmTMB(log(surface_pwS) ~ site_name, 
             ziformula = ~site_name,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h4)

anova(h3, h4)  # h4 is better

# adding a random intercept for site_id
h5 = glmmTMB(log(surface_pwS) ~ site_name + (1|site_id), 
             ziformula = ~site_name,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h5)

anova(h4, h5)  # sig. diff, and h5 is slightly better based on AIC and logLik

# including random intercept for site_id in hurdle part (ziformula)
h6 = glmmTMB(log(surface_pwS) ~ site_name + (1|site_id), 
             ziformula = ~.,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h6)

anova(h4, h5, h6)  # sig. diff, and h5 is slightly better based on AIC and logLik

#---


# is surface pwS sig. diff. between treatments? 
t.test(log(surface_pwS+0.001) ~ treatment, meadow)  # no, but close; p = 0.067

pws.l3 = lm(surface_pwS ~ treatment, meadow)
qqnorm(residuals(pws.l3))

pws.l4 = lm(log(surface_pwS+0.001) ~ treatment, meadow)
qqnorm(residuals(pws.l4))


# trying an LME
pws.m3 = lme(surface_pwS ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m3))
resid_panel(pws.m3)

pws.m4 = lme(log(surface_pwS+0.001) ~ treatment + site_name, random = ~1|site_id, meadow %>% filter(!(is.na(surface_pwS))))
qqnorm(residuals(pws.m4))
resid_panel(pws.m4)


# none of these model types can correctly handle the zeros in the data
# zeros represent when sulfide was seemingly absent from a sample (concentration = 0; no sulfide present)

# test if site has an effect on whether sulfide is absent
h7 = glm(surf_pws_presence ~ site_name, data = meadow_hurdle %>% filter(!(is.na(surface_pwS))), family = "binomial")
summary(h7)  # no, no difference among sites for presence/absence

# test if the positive values of surface PW S differ among sites
h8 = lme(log(surface_pwS+0.001) ~ site_name, random = ~1|site_id,
         data = meadow_hurdle %>% filter(!(is.na(surface_pwS))) %>% filter(!(surf_pws_presence==0)))
summary(h8)  # also no difference among sites for positive values of surface PW S



# try a two-step hurdle model all at once using the glmmTMB::glmmTMB() function

# using raw data and a Gamma distribution with log-link
h9 = glmmTMB(surface_pwS ~ treatment + site_name, 
             ziformula = ~.,
             family = Gamma(link = "log"),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h9)

# using log-transformed data and a gaussian distribution
h10 = glmmTMB(log(surface_pwS) ~ treatment + site_name, 
             ziformula = ~.,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h10)

anova(h9, h10)  # h10 is better

# adding a random intercept for site
h11 = glmmTMB(log(surface_pwS) ~ treatment + site_name + (1|site_id), 
             ziformula = ~treatment + site_name,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h11)

anova(h10, h11)  # h11 is better

# including random intercept for site_id in hurdle part (ziformula)
h12 = glmmTMB(log(surface_pwS) ~ treatment + site_name + (1|site_id), 
             ziformula = ~.,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(surface_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(surface_pwS = if_else(surface_pwS==0, NA_real_, surface_pwS)),
             na.action = na.omit)
summary(h12)

anova(h10, h11, h12)  # h11 is best




#- Rhizome porewater S -# 

# is rhizome pwS sig. diff. between sites? 
aov(log(rhizome_pwS+0.001) ~ site_name, meadow) %>% summary  # no; p = 0.7


# is rhizome pwS diff. between treatments? 
t.test(log(rhizome_pwS+0.001) ~ treatment, meadow)  # no; p = 0.12


# hurdle models are best for surface PW, so use that here for rhizomes too

# test if site has an effect on whether sulfide is absent
pwr.h1 = glm(rhiz_pws_presence ~ site_name, data = meadow_hurdle %>% filter(!(is.na(rhizome_pwS))), family = "binomial")
summary(pwr.h1)  # no, no difference among sites for presence/absence

# test if the positive values of rhizome PW S differ among sites
pwr.h2 = lme(log(rhizome_pwS+0.001) ~ site_name, random = ~1|site_id,
         data = meadow_hurdle %>% filter(!(is.na(rhizome_pwS))) %>% filter(!(rhiz_pws_presence==0)))
summary(pwr.h2)  # also no difference among sites for positive values of rhizome PW S





pwr.h3 = glmmTMB(rhizome_pwS ~ treatment + site_name + (1|site_id), 
             ziformula = ~treatment + site_name,
             family = Gamma(link = "log"),,
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(rhizome_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(rhizome_pwS = if_else(rhizome_pwS==0, NA_real_, rhizome_pwS)),
             na.action = na.omit)
summary(pwr.h3)


pwr.h4 = glmmTMB(log(rhizome_pwS) ~ treatment + site_name + (1|site_id), 
             ziformula = ~treatment + site_name,
             family = gaussian(),
             data = meadow_hurdle %>% 
                # remove the true NA's in the data
                filter(!(is.na(rhizome_pwS))) %>%
                # replace all values of 0 with NA (b/c NA's are used for the hurdle part of the model)
                mutate(rhizome_pwS = if_else(rhizome_pwS==0, NA_real_, rhizome_pwS)),
             na.action = na.omit)
summary(pwr.h4)

anova(pwr.h3, pwr.h4)



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







