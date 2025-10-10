#~~~
# Script for processing spectrophotometer data and calculating sulfide concentration from spatial porewater samples
# By: R. Johnson
#
# Spatial porewater sulfide - surface and rhizome depths
# Summer 2024 Florida Keys sampling
#~~~


setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance")

library(tidyverse)

# need to run "data_S-std-curves" script first
source("C:/Users/rajohnson6/Desktop/Local-Repos/Disturbance-Edge-Effects/Data-Scripts/data_S-std-curves.R")


# naming convention (examples for object names, so I don't forget by the time I have new data/runs to add here)
# S1_s01 -> site 1, surface porewater, run 1
# S1_r01 -> site 1, rhizome porewater, run 1
# S1_r02 -> site 1, rhizome porewater, run 2

# S2_s01 -> site 2, surface porewater, run 1
# S3_r02 -> site 3, rhizome porewater, run 2


# Function to check standard concentrations for each run
check_stds = function(.dat, .std_curve) {
   
   .dat %>%
      filter(str_detect(vial, pattern = "L")) %>%
      # correct for blank absorbance
      mutate(abs = abs_667 - (.dat %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean)) %>%
      # concentration
      mutate(stdS = calc_S_conc(abs, .std_curve),
             expected = parse_number(sample_id),
             diff = expected - stdS,
             perc_diff = (diff / expected) * 100) %>%
      print(n=Inf)
}

# Function to remove Zeros, Blanks, Standards, and Checks vials from data
rm_zbsc = function(.dat) {
   
   .dat %>%
      filter(!(sample_id=="Zero" | sample_id=="Blank"),
             !(str_detect(sample_id, pattern="chk")),
             !(str_detect(sample_id, pattern="L")))
}

# Function to process datasheet and calculate sulfide concentration in microcentrifuge vial
calc_vial_S = function(.processed, .raw, .std_curve) {
   
   .processed %>%
      # correct absorbance
      mutate(
         # for blanks
         abs_blk_corr = abs_667 - (.raw %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean),
         # for post-color dilution
         abs_corr = abs_blk_corr * dilution_post) %>%
      # remove any samples that were too low or too high and need to be re-run with a different pre-color dilution
      filter(!(flag %in% c("L", "H"))) %>%
      # remove sample dupes
      filter(!(str_detect(sample_id, pattern="dup"))) %>%
      # sulfide concentration in microcentrifuge vial that diamine reagent was added to (units = uM)
      mutate(vial_S_uM = calc_S_conc(abs_corr, .std_curve)) %>%
      # for samples that were below detection limit, concentration = 0
      mutate(vial_S_uM = replace(vial_S_uM, str_detect(.$flag, pattern="D"), 0))
}
   


#--
# Craig Key sites - spec data
#--

# Sites S1.1, S1.2, S1.3

#-- Surface Porewater, run 1 --#
# Standard curve to use: April 2025
raw_S1_s01 = read_csv("Data/Spec Data/2025.04.06 - FLK24_spatial porewater_S1.1-1.3_surface.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S1_s01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S1_s01 = rm_zbsc(raw_S1_s01)

# check agreement between sample dupes
S1_s01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S1.1-D-5.0-S: 0.364/0.374 = 0.97; difference acceptable
   # S1.2-D-5.0-S: 0.009/0.01 = 0.9; disregard, samples needed to be rerun
   # S1.3-D-5.0-S: 0.035/0.044 = 0.795; disregard, samples needed to be rerun

# Sulfide concentration in vials (units = uM)
S1_s01 = calc_vial_S(S1_s01, raw_S1_s01, std_apr25)


#-- Surface Porewater, run 2 --#
# Standard curve to use: April 2025
raw_S1_s02 = read_csv("Data/Spec Data/2025.04.13 - FLK24_spatial porewater_S1.1-1.3 reruns_surface.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S1_s02, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S1_s02 = rm_zbsc(raw_S1_s02)

# check agreement between sample dupes
S1_s02 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # no dupes

# Sulfide concentration in vials (units = uM)
S1_s02 = calc_vial_S(S1_s02, raw_S1_s02, std_apr25)


#-- Rhizome Porewater, run 1 --#
# Standard curve to use: April 2025
raw_S1_r01 = read_csv("Data/Spec Data/2025.07.12 - FLK24_spatial porewater_S1.1-1.3_rhizome.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S1_r01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S1_r01 = rm_zbsc(raw_S1_r01)

# check agreement between sample dupes
S1_r01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S1.2-D-5.0-R: 0.097 / 0.115 = 0.843  # ??
   # S1.3-D-5.0-R: 0.204 / 0.212 = 0.962  # acceptable

# Sulfide concentration in vials (units = uM)
S1_r01 = calc_vial_S(S1_r01, raw_S1_r01, std_apr25)



#--
# Little Conch Key sites - spec data
#--

# Sites S2.1, S2.2, S2.3

#-- Surface Porewater, run 1 --#
# Standard curve to use: April 2025
raw_S2_s01 = read_csv("Data/Spec Data/2025.04.13 - FLK24_spatial porewater_S2.1-2.3_surface.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S2_s01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S2_s01 = rm_zbsc(raw_S2_s01)

# check agreement between sample dupes
S2_s01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S2.1-D-5.0-S: 0.026/0.027 = 0.96; disregard, samples needed to be rerun
   # S2.2-D-5.0-S: 0.407/0.416 = 0.98; difference acceptable
   # S2.3-D-5.0-S: 0.54/0.519 = 1.04; difference acceptable

# Sulfide concentration in vials (units = uM)
S2_s01 = calc_vial_S(S2_s01, raw_S2_s01, std_apr25) %>%
   # remove the sample that was rerun with post-dilution (effect of post-dilution is non-linear, this sample needs to be rerun)
   filter(!(dilution_post > 1))


#-- Surface Porewater, run 2 --#
# Standard curve to use: April 2025
raw_S2_s02 = read_csv("Data/Spec Data/2025.07.12 - FLK24_spatial porewater_S2.1-3.2 reruns_surface.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S2_s02, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S2_s02 = rm_zbsc(raw_S2_s02)

# check agreement between sample dupes
S2_s02 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S2.3-D-4.0-S: 0.606/0.618 = 0.98; difference acceptable

# Sulfide concentration in vials (units = uM)
S2_s02 = calc_vial_S(S2_s02, raw_S2_s02, std_apr25)


#-- Rhizome Porewater, run 1 --#
# Standard curve to use: April 2025
raw_S2_r01 = read_csv("Data/Spec Data/2025.07.26 - FLK24_spatial porewater_S2.1-2.3, S3.2_rhizome.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S2_r01, std_apr25)

# --> all standards were a bit off today. compare to std. values from recent runs and determine how far off.
#      may need to apply correction factor to absorbance values for all samples run on 2025.07.26

# --> need to remove S3.2 data when processing this df. process this same sheet for S3.2 under Anne's Beach below

# { need to finish processing this }


## --> there are also re-runs in the file "2025.07.26 - FLK24_porewater reruns from 07.26 run.csv"



#--
# Anne's Beach sites - spec data
#--

# Sites S3.1, S3.2

#-- Surface and Rhizome Porewater, run 1 --#
# Standard curve to use: April 2025
raw_S3_01 = read_csv("Data/Spec Data/2025.07.05 - FLK24_spatial porewater_S3.1-3.2_surf and rhiz.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S3_01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S3_01 = rm_zbsc(raw_S3_01)

# check agreement between sample dupes
S3_01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # all look good

# Sulfide concentration in vials (units = uM)
S3_01 = calc_vial_S(S3_01, raw_S3_01, std_apr25)


# Surface samples
S3_s01 = S3_01 %>% filter(str_detect(sample_id, pattern="-S"))

# Rhizome samples
S3_r01 = S3_01 %>% filter(str_detect(sample_id, pattern="-R"))


#-- Rhizome Porewater, run 2 --#
# Standard curve to use: April 2025
raw_S3_r02 = read_csv("Data/Spec Data/2025.07.26 - FLK24_spatial porewater_S2.1-2.3, S3.2_rhizome.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_S3_r02, std_apr25)

# --> all standards were a bit off today. compare to std. values from recent runs and determine how far off.
#      may need to apply correction factor to absorbance values for all samples run on 2025.07.26

# --> need to remove S2.1-S2.3 data when processing this df. only process S3.2 data here

# { need to finish processing this }


## --> there are also re-runs in the file "2025.07.26 - FLK24_porewater reruns from 07.26 run.csv"




#--
# Porewater Sulfide Concentration
#--

# Porewater sample data
spatial_pw_sample_data = read.csv("Data/FLK24_spatial_porewater.csv")


# Combine spec runs and calculate sulfide concentration (units = uM)
fk_pw_spatial = bind_rows(S1_s01, S1_s02, S1_r01, S2_s01, S2_s02, S3_s01, S3_r01) %>%
   # correct measured sulfide concentration for any dilution prior to adding diamine reagent (units = uM)
   mutate(scint_S_uM = vial_S_uM * dilution_pre)


#- Sulfide concentration in original porewater
fk_pw_spatial = fk_pw_spatial %>%
   select(subsample_id = sample_id, scint_S_uM) %>%
   # make sample ID structure match with sample data file
   mutate(subsample_id = paste0("FLK24-", subsample_id)) %>% 
   # combine with sample data
   left_join(spatial_pw_sample_data) %>%
   relocate(scint_S_uM, .after=last_col()) %>%
   # calculate porewater S concentration (units = uM)
   mutate(
      # total S in vial (concentration times total aqueous volume)
      scint_S_umol = scint_S_uM * ((sample_vol_cc + znac_vol_cc) / 1000),
      # concentration of S in porewater
      porewater_S_uM = scint_S_umol / (sample_vol_cc / 1000))



