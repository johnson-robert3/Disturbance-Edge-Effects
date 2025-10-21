

# coefficient of variation (by treatment)

fk_pw_spatial %>%
   summarize(cv_pw = sd(porewater_S_uM, na.rm=T)/mean(porewater_S_uM, na.rm=T), .by=c(site, treatment))


library(nlme)
library(car)


# test significant effects of site and treatment

# sulfide
mods = lme(porewater_S_uM ~ treatment * site, random = ~1|site_id, data = fk_pw_spatial)

Anova(mods)


# dbd
modd = lme(dbd ~ treatment * site, random = ~1|site_id, data = meadow %>% filter(!site==3))

Anova(modd)


# OM
modo = lme(perc_om ~ treatment * site, random = ~1|site_id, data = meadow %>% filter(!site==3))

Anova(modo)



# values

meadow %>%
   summarise(mean(dbd, na.rm=T), .by=treatment)

meadow %>%
   summarise(mean(perc_om, na.rm=T), .by=treatment)


fk_pw_spatial %>%
   summarize(mean(porewater_S_uM, na.rm=T), sd(porewater_S_uM, na.rm=T), .by=treatment)


fk_pw_spatial %>%
   summarize(min(porewater_S_uM, na.rm=T), max(porewater_S_uM, na.rm=T), .by=treatment)


fk_pw_spatial %>%
   summarize(mean(porewater_S_uM, na.rm=T), sd(porewater_S_uM, na.rm=T), .by=site)


fk_pw_spatial %>%
   summarize(mean(porewater_S_uM, na.rm=T), sd(porewater_S_uM, na.rm=T), .by=c(site, treatment))






