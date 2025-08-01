# Prelim figs for looking at spatial porewater sulfide



fk_breaks = c('disturbed', 'undisturbed')
# fk_site_cols = c('LP' = '#C63D2F', 'CG' = '#FF9B50', 'CH' = '#002B5B', 'CW' = '#159895')
fk_site_cols = c('1' = '#C63D2F', '2' = '#FF9B50', '3' = '#002B5B')
fk_shapes = c('disturbed' = 22, 'undisturbed' = 15)



# box plots by site (pooled patches)
windows(height=3.5, width=5)
ggplot(fk_pw_spatial %>%
          mutate(site = factor(site, levels = c('1', '2')))) +
   #
   geom_boxplot(aes(x = site, y = porewater_S_uM, group = interaction(site, treatment), color = treatment)) +
   # 
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_color_manual(breaks = fk_breaks, 
                      values = c('disturbed' = 'cornflowerblue', 'undisturbed' = 'black')) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# box plot by patch (site 1, Craig)
windows(height=3.5, width=5)
ggplot(fk_pw_spatial %>%
          filter(site==1) %>%
          mutate(site = factor(site),
                 patch = factor(patch))) +
   #
   geom_boxplot(aes(x = site, y = porewater_S_uM, color = treatment)) +
   # 
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_color_manual(breaks = fk_breaks, 
                      values = c('disturbed' = 'cornflowerblue', 'undisturbed' = 'black')) +
   facet_wrap(facets = vars(patch)) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# box plot by patch (site 2, Little Conch)
windows(height=3.5, width=5)
ggplot(fk_pw_spatial %>%
          filter(site==2) %>%
          mutate(site = factor(site),
                 patch = factor(patch))) +
   #
   geom_boxplot(aes(x = site, y = porewater_S_uM, color = treatment)) +
   # 
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_color_manual(breaks = fk_breaks, 
                      values = c('disturbed' = 'cornflowerblue', 'undisturbed' = 'black')) +
   facet_wrap(facets = vars(patch)) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# box plot by patch (both sites)
windows(height=3.5, width=6)
ggplot(fk_pw_spatial %>%
          mutate(site = factor(site),
                 patch = factor(patch))) +
   #
   geom_boxplot(aes(x = patch, y = porewater_S_uM, color = treatment, group = interaction(patch, treatment))) +
   # 
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_color_manual(breaks = fk_breaks, 
                      values = c('disturbed' = 'cornflowerblue', 'undisturbed' = 'black')) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# based on transect position (distance from edge), all together
windows(height=3.5, width=5)
ggplot(fk_pw_spatial %>%
          mutate(transect_location_m = if_else(treatment=="disturbed", transect_location_m * -1, transect_location_m))) +
   #
   geom_line(aes(x = transect_location_m, y = porewater_S_uM, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = porewater_S_uM, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   


# based on transect position (distance from edge), by site
windows(height=3.5, width=8)
ggplot(fk_pw_spatial %>%
          mutate(transect_location_m = if_else(treatment=="disturbed", transect_location_m * -1, transect_location_m))) +
   #
   geom_line(aes(x = transect_location_m, y = porewater_S_uM, group = interaction(site_id, treatment), linetype = treatment, color = site_id), alpha=0.5) +
   geom_smooth(aes(x = transect_location_m, y = porewater_S_uM, group = treatment), color="black", linewidth=1.5) +
   geom_point(aes(x = transect_location_m, y = porewater_S_uM, fill = site_id), shape = 21, size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   










