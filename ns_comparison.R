library(tidyverse)
library(targets)

tar_load_everything()

ns_matches <- c(
  "card_1992_ilrr_ca",
  "card_1992_ilrr_regional",
  "kk_1992_ilrr",
  "kt_1995_jbes",
  "cf_1996_jhr",
  "bcw_2000_jole",
  "zavodny_2000_le",
  "nsw_2004_jhr",
  "dnr_2007_ilrr",
  "nn_2007_jhr",
  "oz_2008_ilrr",
  "sabia_2008_jpam",
  "abc_2009_le",
  "sabia_2009_ir",
  "sabia_2009_jlr",
  "thompson_2009_ilrr",
  "dlr_2010_restat",
  "adr_2011_ir",
  "abc_2012_bjir",
  "sbh_2012_ilrr",
  "abc_2013_le",
  "em_2014_sej",
  "abc_2015_izajle",
  "hkz_2015_ir",
  "dlr_2016_jole",
  "gs_2016_ilrr",
  "lhr_2016_labour",
  "adrz_2017_ilrr",
  "totty_2017_ei",
  "cdlz_2019_qje",
  "monras_2019_jole",
  "manning_2021_jep",
  "bds_2021_jole",
  "dm_2021_qje",
  "ghks_2021_jole"
)

owe_data %>% 
  mutate(ns_included = if_else(study_id %in% ns_matches, 1, 0)) %>% 
  filter(ns_included == 1) %>% 
  skimr::skim(owe_b)
  
owe_data %>% 
  filter(published == 1, country == "US", year <= 2021) %>% 
  skimr::skim(owe_b)
