#
# Script to look at potential drivers of sulfide concentration
#


library(nlme)



#- Rhizome Sulfide

## significant (but adj R2 = 0.045)
lm(log(rhizome_pwS) ~ treatment + porosity, meadow) %>% summary


mod = lme(log(rhizome_pwS) ~ treatment + porosity, random = ~1|site_id,
          data = meadow %>% filter(!(is.na(rhizome_pwS))))
summary(mod)
# same results as simpler linear regression


# trying with additional variables from PCA
lm(log(rhizome_pwS) ~ treatment + porosity + burrow_density + total_sg_biomass, meadow) %>% summary


#- Rhizome sulfide vs Surface sulfide

lm(log(rhizome_pwS) ~ log(surface_pwS) + treatment, meadow) %>% summary
## sulfide at rhiz depth is significantly related to the surface depth; p < 0.01
## no difference between treatments; p = 0.5


