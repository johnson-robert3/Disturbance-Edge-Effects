
#
# Principle Components Analysis for potential drivers of sulfide concentration
#


# library(vegan)
library(FactoMineR)
library(factoextra)

set.seed(123)


# create dataset for PCA analysis
pdat = meadow %>%
   select(site_name, site_id, treatment, transect_location_m,
          Tt_density, total_ma_density, burrow_density, total_sg_biomass,
          dbd, perc_om, porosity,
          surface_pwS, rhizome_pwS) %>%
   arrange(site_id, treatment, transect_location_m) %>%
   # remove data w/ missing sulfide
   filter(!(if_any(contains("_pwS"), is.na))) %>%
   # log
   mutate(across(contains("_pwS"), ~log(.)))


# pdat = meadow %>%
#    select(site_name, treatment, transect_location_m,
#           Tt_density, total_ma_density, burrow_density, total_sg_biomass, 
#           dbd, perc_om, porosity) %>%
#    arrange(site_name, treatment, transect_location_m)



# subset data to use for PCA
# remove ID columns and response variables

# pdat_sub = pdat %>%
#    select(-c(site_name, site_id, treatment, surface_pwS, rhizome_pwS))

pdat_sub = pdat %>%
   select(-c(site_id, transect_location_m, total_ma_density))



# Try a PCA the FactoMineR way

# pca_results <- PCA(pdat_sub, graph = FALSE)

# pca_results <- PCA(pdat[, -1:-2], graph = FALSE)


pca_results <- PCA(pdat_sub, 
                   # supplemental qualitative variables (site and treatment)
                   quali.sup=1:2, 
                   # supplemental quantitative variables (porewater sulfid)
                   quanti.sup=9:10, 
                   graph=FALSE)



# view the scree plot for important PCs
fviz_screeplot(pca_results, addlabels = TRUE)


# pca biplot
windows(width=5, height=3.5)
fviz_pca_biplot(pca_results, habillage=as.factor(pdat$treatment), 
                addEllipses=TRUE, ellipse.level=0.95, label="var")




