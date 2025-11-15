#~~
# Principle Components Analysis for potential variable correlations with sulfide concentration
#
#~~


# library(vegan)
library(FactoMineR)
library(factoextra)


set.seed(123)


# create dataset for PCA analysis
pdat = meadow %>%
   select(site_name, site_id, treatment, transect_location_m,
          Tt_density, total_ma_density, burrow_density, total_sg_biomass, blade_length, 
          dbd, perc_om, porosity,
          surface_pwS, rhizome_pwS) %>%
   arrange(site_id, treatment, transect_location_m) %>%
   # convert NAN to 0
   mutate(blade_length = if_else(blade_length=="NaN", 0, blade_length)) %>%
   # remove data w/ missing sulfide
   filter(!(if_any(contains("_pwS"), is.na))) %>%
   # log-transform sulfide
   mutate(across(contains("_pwS"), ~log(.)))



# subset data to use for PCA

pdat_sub = pdat %>%
   # remove unnecessary or unneeded variables
   select(-c(site_id, transect_location_m, total_ma_density)) %>%
   # relocate response variables so they are always in the same column numbers for reference in the PCA function
   relocate(surface_pwS, rhizome_pwS, .after=treatment) #%>%
   # remove response variables to be able to make a biplot fig without these arrows
   # select(-surface_pwS, -rhizome_pwS)


# subset to view just Anne's Beach site
pdat_sub_a = pdat_sub %>% filter(site_name=="Annes Beach")

# subset to view without Anne's Beach site
pdat_sub_cl = pdat_sub %>% filter(!(site_name=="Annes Beach"))



# Try a PCA the FactoMineR way

# pca_results <- PCA(pdat_sub, graph = FALSE)
# pca_results <- PCA(pdat[, -1:-2], graph = FALSE)

pca_results <- PCA(pdat_sub, 
                   # supplemental qualitative variables (site and treatment)
                   quali.sup=1:2, 
                   # supplemental quantitative variables (porewater sulfide)
                   quanti.sup=3:4,
                   graph=FALSE)



# view the scree plot for important PCs
fviz_screeplot(pca_results, addlabels = TRUE)


# PCA biplot by treatment
windows(width=5, height=3.5)
fviz_pca_biplot(pca_results, 
                habillage = as.factor(pdat$treatment),  # specify the variable by which to split and color data
                addEllipses=TRUE, ellipse.level=0.95, 
                geom.ind="point", geom.var="arrow", col.var="black", label="none", title="") +
   scale_shape_manual(values = c(16,16)) +
   scale_fill_manual(values = vu_fill) +
   scale_color_manual(values = vu_fill) +
   theme(axis.text = element_text(color='black', size=rel(0.8)),
         axis.title = element_text(color='black'))

ggsave(file = "Figures/CERF 2025/PCA_by-trt.png")
# ggsave(file = "Figures/CERF 2025/PCA_by-trt_no-S-arrows.png")  # remove sulfide vars from pdat_sub to create this
# ggsave(file = "Figures/CERF 2025/PCA_by-trt_with-var-names.png")  # change label argument within fviz_pca_biplot to create this




# just data points (no arrows)
windows(width=5, height=3.5)
fviz_pca_ind(pca_results, 
             habillage = as.factor(pdat$treatment),
             addEllipses=TRUE, ellipse.level=0.95,
             geom.ind="point", title="") +
   scale_shape_manual(values = c(16,16)) +
   scale_fill_manual(values = vu_fill) +
   scale_color_manual(values = vu_fill) +
   theme(axis.text = element_text(color='black', size=rel(0.8)),
         axis.title = element_text(color='black'))

ggsave(file = "Figures/CERF 2025/PCA_by-trt_just-points.png")
         



# PCA biplot by site
windows(width=5, height=3.5)
fviz_pca_biplot(pca_results, habillage=as.factor(pdat$site_name), 
                addEllipses=TRUE, ellipse.level=0.95, label="var")



