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
std_conc_chk = function(.dat, .std_curve) {
   
   .dat %>%
      filter(str_detect(vial, pattern = "L")) %>%
      # correct for blank absorbance
      mutate(abs = abs_667 - (.dat %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean)) %>%
      # concentration
      mutate(stdS = calc_h2s_conc(abs, .std_curve),
             expected = parse_number(sample_id),
             diff = expected - stdS,
             percdiff = (diff / expected) * 100) %>%
      print(n=Inf)
}


#--
# Craig Key sites - spec data
#--

# Sites S1.1, S1.2, S1.3

#- Surface Porewater, run 1
# Standard curve to use: April 2025
raw_S1_s01 = read_csv("Data/Spec Data/2025.04.06 - FLK24_spatial surface porewater_S1.1-1.3.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
std_conc_chk(raw_S1_s01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S1_s01 = raw_S1_s01 %>%
   # remove Zero, blanks, standards, and checks
   filter(!(sample_id=="Zero" | sample_id=="Blank"),
          !(str_detect(sample_id, pattern="chk")),
          !(str_detect(sample_id, pattern="L")))

# check agreement between sample dupes
S1_s01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S1.1-D-5.0-S: 0.364/0.374 = 0.97; difference acceptable
   # S1.2-D-5.0-S: 0.009/0.01 = 0.9; disregard, samples needed to be rerun
   # S1.3-D-5.0-S: 0.035/0.044 = 0.795; disregard, samples needed to be rerun

# Sulfide concentration in vials (units = uM)
S1_s01 = S1_s01 %>%
   # correct absorbance
   mutate(
      # for blanks
      abs_blk_corr = abs_667 - (raw_S1_s01 %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean),
      # for post-color dilution
      abs_corr = abs_blk_corr * dilution_post) %>%
   # remove the samples that were too low and need to be re-run with a lower pre-color dilution
   anti_join(S1_s01 %>% filter(str_detect(notes, pattern="rerun at lower df"))) %>%
   # remove sample dupes
   filter(!(str_detect(sample_id, pattern="dup"))) %>%
   # sulfide concentration in microcentrifuge vial that diamine reagent was added to (units = uM)
   mutate(vial_S_uM = calc_h2s_conc(abs_corr, std_apr25))


#- Surface Porewater, run 2
# Standard curve to use: April 2025
raw_S1_s02 = read_csv("Data/Spec Data/2025.04.13 - FLK24_spatial surface porewater_S1.1-1.3 reruns.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
std_conc_chk(raw_S1_s02, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S1_s02 = raw_S1_s02 %>%
   # remove Zero, blanks, standards, and checks
   filter(!(sample_id=="Zero" | sample_id=="Blank"),
          !(str_detect(sample_id, pattern="chk")),
          !(str_detect(sample_id, pattern="L")))

# check agreement between sample dupes
S1_s02 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # no dupes

# Sulfide concentration in vials (units = uM)
S1_s02 = S1_s02 %>%
   # correct absorbance
   mutate(
      # for blanks
      abs_blk_corr = abs_667 - (raw_S1_s02 %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean),
      # for post-color dilution
      abs_corr = abs_blk_corr * dilution_post) %>%
   # sulfide concentration in microcentrifuge vial that diamine reagent was added to (units = uM)
   mutate(vial_S_uM = calc_h2s_conc(abs_corr, std_apr25)) %>%
   # for samples that were below detection limit, concentration = 0
   mutate(vial_S_uM = replace(vial_S_uM, str_detect(.$notes, pattern="DL"), 0))



#--
# Little Conch Key sites - spec data
#--

# Sites S2.1, S2.2, S2.3

#- Surface Porewater, run 1
# Standard curve to use: April 2025
raw_S2_s01 = read_csv("Data/Spec Data/2025.04.13 - FLK24_spatial surface porewater_S2.1-2.3.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
std_conc_chk(raw_S2_s01, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
S2_s01 = raw_S2_s01 %>%
   # remove Zero, blanks, standards, and checks
   filter(!(sample_id=="Zero" | sample_id=="Blank"),
          !(str_detect(sample_id, pattern="chk")),
          !(str_detect(sample_id, pattern="L")))

# check agreement between sample dupes
S2_s01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))
   # S2.1-D-5.0-S: 0.026/0.027 = 0.96; disregard, samples needed to be rerun
   # S2.2-D-5.0-S: 0.407/0.416 = 0.98; difference acceptable
   # S2.3-D-5.0-S: 0.54/0.519 = 1.04; difference acceptable

# Sulfide concentration in vials (units = uM)
S2_s01 = S2_s01 %>%
   # correct absorbance
   mutate(
      # for blanks
      abs_blk_corr = abs_667 - (raw_S2_s01 %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean),
      # for post-color dilution
      abs_corr = abs_blk_corr * dilution_post) %>%
   # remove the samples that were too low and need to be re-run with a lower pre-color dilution
   anti_join(S2_s01 %>% filter(str_detect(notes, pattern="rerun at lower df"))) %>%
   # remove the samples that were too high and need to be re-run with a higher pre-color dilution
   anti_join(S2_s01 %>% filter(str_detect(notes, pattern="rerun at higher df"))) %>%
   # remove sample dupes
   filter(!(str_detect(sample_id, pattern="dup"))) %>%
   # remove the sample that was rerun with post-dilution (effect of post-dilution is non-linear, this sample needs to be rerun)
   anti_join(S2_s01 %>% filter(str_detect(notes, pattern="post-dilution"))) %>%
   # sulfide concentration in microcentrifuge vial that diamine reagent was added to (units = uM)
   mutate(vial_S_uM = calc_h2s_conc(abs_corr, std_apr25)) %>%
   # for samples that were below detection limit, concentration = 0
   mutate(vial_S_uM = replace(vial_S_uM, str_detect(.$notes, pattern="DL"), 0))



#--
# Porewater Sulfide Concentration
#--

# Porewater sample data
spatial_pw_sample_data = read.csv("Data/FLK24_spatial_porewater.csv")


# Combine spec runs and calculate sulfide concentration (units = uM)
fk_pw_spatial = bind_rows(S1_s01, S1_s02, S2_s01) %>%
   # correct measured sulfide concentration for any dilution prior to adding diamine reagent (units = uM)
   mutate(scint_S_uM = vial_S_uM * dilution_pre)


#- Sulfide concentration in original porewater
fk_pw_spatial = fk_pw_spatial %>%
   select(subsample_id = sample_id, scint_S_uM) %>%
   # make sample ID structure match with sample data file
   mutate(subsample_id = paste("FLK24-", subsample_id, sep="")) %>% 
   # combine with sample data
   left_join(spatial_pw_sample_data) %>%
   # calculate porewater S concentration (units = uM)
   mutate(
      # total S in vial (concentration times total aqueous volume)
      scint_S_umol = scint_S_uM * ((sample_vol_cc + znac_vol_cc) / 1000),
      # concentration of S in porewater
      porewater_S_uM = scint_S_umol / (sample_vol_cc / 1000))



