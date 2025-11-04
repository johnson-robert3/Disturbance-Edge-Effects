

# coefficient of variation (by treatment)

fk_pw_spatial %>% # this is no longer valid, as it doesn't separate surface and rhizome depth porewater samples
   summarize(cv_pw = sd(porewater_S_uM, na.rm=T)/mean(porewater_S_uM, na.rm=T), .by=c(site, treatment)) 


library(nlme)
library(car)


# test significant effects of site and treatment

# create a model dataset
mod_dat = meadow 



# Sulfide
# mods = lme(porewater_S_uM ~ treatment * site, random = ~1|site_id, data = fk_pw_spatial) # version used for CERF abstract
# Anova(mods)

mods = lme(log(surface_pwS+0.001) ~ treatment * site, random = ~1|site_id, data = mod_dat %>% filter(!is.na(surface_pwS)))
Anova(mods)

mods = lme(log(rhizome_pwS+0.001) ~ treatment * site, random = ~1|site_id, data = mod_dat %>% filter(!is.na(rhizome_pwS)))
Anova(mods)


# DBD
# modd = lme(dbd ~ treatment * site, random = ~1|site_id, data = meadow %>% filter(!site==3)) # version used for CERF abstract
# Anova(modd)

modd = lme(dbd ~ treatment * site, random = ~1|site_id, data = mod_dat)
Anova(modd)


# OM
# modo = lme(perc_om ~ treatment * site, random = ~1|site_id, data = meadow %>% filter(!site==3)) # version used for CERF abstract
# Anova(modo)

modo = lme(perc_om ~ treatment * site, random = ~1|site_id, data = mod_dat)
Anova(modo)



# values

mod_dat %>%
   summarise(mean(dbd, na.rm=T), .by=treatment)

mod_dat %>%
   summarise(mean(perc_om, na.rm=T), .by=treatment)


mod_dat %>%
   summarize(mean(surface_pwS, na.rm=T), sd(surface_pwS, na.rm=T), .by=treatment)
mod_dat %>%
   summarize(mean(rhizome_pwS, na.rm=T), sd(rhizome_pwS, na.rm=T), .by=treatment)


mod_dat %>%
   summarize(min(surface_pwS, na.rm=T), max(surface_pwS, na.rm=T), .by=treatment)
mod_dat %>%
   summarize(min(rhizome_pwS, na.rm=T), max(rhizome_pwS, na.rm=T), .by=treatment)


mod_dat %>%
   summarize(mean(surface_pwS, na.rm=T), sd(surface_pwS, na.rm=T), .by=site)
mod_dat %>%
   summarize(mean(rhizome_pwS, na.rm=T), sd(rhizome_pwS, na.rm=T), .by=site)


mod_dat %>%
   summarize(mean(surface_pwS, na.rm=T), sd(surface_pwS, na.rm=T), .by=c(site, treatment))
mod_dat %>%
   summarize(mean(rhizome_pwS, na.rm=T), sd(rhizome_pwS, na.rm=T), .by=c(site, treatment))



# both surface and rhizome depths for porewater S values are heavily skewed and need to be natural-log transformed to achieve normal dist.

test = meadow %>% filter(!is.na(surface_pwS)) %>% filter(!is.na(rhizome_pwS))


summary(lm(log(rhizome_pwS+0.001) ~ log(surface_pwS+0.001), data = test))



