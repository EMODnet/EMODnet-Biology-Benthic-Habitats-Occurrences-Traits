# Read in and process traits data from Biotic
# Data provided by Dan Lear to TJW 08/06/2020
# Biotic is at http://www.marlin.ac.uk/biotic/

# Aim is to get a list of species with known substrate preferences and habits in order to match OBIS occurrences to habitat maps

# Load packages
library(tidyverse)
library(worrms)
library(robis)
library(naniar)

# Read in datasets
env_pos <- read_csv("data/raw_data/biotic/bioticEnvPos.csv")
feed_method <- read_csv("data/raw_data/biotic/bioticFeedingMethod.csv")
habit <- read_csv("data/raw_data/biotic/bioticHabit.csv")
substratum <- read_csv("data/raw_data/biotic/bioticSubstratum.csv")
dev_mech <- read_csv("data/raw_data/biotic/bioticDevMech.csv")


# Substratum matching
# Create a lookup table of substrates
substrate_values <- tibble(
  substratum = unique(substratum$substratum)
) %>% mutate(substratum_code = case_when(
  substratum == "Large to very large boulders" ~ "Large_VLarge_Boulders",
  substratum == "Small boulders" ~ "Small_Boulders",
  substratum == "Muddy gravel" ~ "Muddy_gravel",
  substratum == "Muddy sand" ~ "Muddy_sand",
  substratum == "Sandy mud" ~ "Sandy_mud",
  substratum == "Coarse clean sand" ~ "Coarse_sand",
  substratum == "Fine clean sand" ~ "Fine_sand",
  substratum == "Other species (see additional information)" ~ "Other_species",
  substratum == "Artificial (e.g. metal/wood/concrete)" ~ "Artificial",
  substratum == "Insufficient information" ~ "No_Info",
  substratum == "Gravel / shingle" ~ "Gravel_shingle",
  substratum == "Salt marsh" ~ "Salt_marsh",
  substratum == "Biogenic reef" ~ "Biogenic_reef",
  substratum == "Under boulders" ~ "Under_boulders",
  substratum == "Crevices / fissures" ~ "Crevices",
  substratum == "Water column (pelagic)" ~ "Pelagic",
  substratum == "No preference" ~ "No_preference",
  substratum == "Muddy gravelly sand" ~ "Mud_grav_sand",
  substratum == "Sandy gravelly mud" ~ "Sand_grav_mud",
  substratum == "See additional information" ~ "No_Info",
  substratum == "Gravelley sand" ~ "Gravel_sand",
  substratum == "Muddy sandy gravel" ~ "Mud_sand_gravel",
  TRUE ~ substratum
))

# replace long values with abbreviated values in substratum dataframe
substratum <- left_join(substratum, substrate_values, by = "substratum") %>% 
  select(-substratum)

# Get rid of uninformative records
substratum <- substratum %>% filter(!(substratum_code %in% c("No_Info", "No_preference")))

substratum_wide <- substratum %>% 
  count(SpeciesName, substratum_code) %>%
  pivot_wider(names_from = substratum_code, values_from = n)

# convert to one row per species, one column per trait modality
env_pos_wide <- env_pos %>%
  count(SpeciesName, envpos) %>%
  pivot_wider(names_from = envpos, values_from = n)

feed_method_wide <- feed_method %>% 
  count(SpeciesName, feedingmethod) %>%
  pivot_wider(names_from = feedingmethod, values_from = n)

habit_wide <- habit %>% 
  count(SpeciesName, Habit) %>%
  pivot_wider(names_from = Habit, values_from = n)


# get full species list
biotic_species <- unique(c(
  pull(env_pos_wide, SpeciesName),
  pull(feed_method_wide, SpeciesName),
  pull(habit_wide, SpeciesName),
  pull(substratum_wide, SpeciesName)
))

# check WoRMS taxonomy
biotic_aphias <- wm_name2id_(biotic_species)
biotic_aphias <- biotic_aphias %>% enframe() %>% mutate(value = unlist(value))

# check those that did not match
sum(biotic_aphias$value == "-999")
sum(biotic_aphias$value == "Not found")

# Quite a few - easiest solution here is to use the WoRMS taxon match tool at http://www.marinespecies.org/aphia.php?p=match
# Write species names to file
write_csv(select(biotic_aphias, name), "data/derived_data/biotic_aphias_to_check.txt")
# Read in matched results
biotic_aphias_matched <- read_tsv("data/derived_data/biotic_aphias_to_check_matched.txt")

# remove unmatched species, plus any at rank above species
biotic_aphias_matched <- biotic_aphias_matched %>% 
  filter(!is.na(AphiaID_accepted) & !is.na(Species)) %>% 
  select(-c(Subgenus, Species, Subspecies))

# add functional group, using functional group matching function in get_worms_fgrp.R (from https://github.com/tomjwebb/WoRMS-functional-groups)
spp_attr <- biotic_aphias_matched %>%
  mutate(aphia = AphiaID_accepted) %>% 
  group_by(AphiaID_accepted) %>% 
  group_map(~ get_worms_fgrp(AphiaID = .x$aphia)) %>% 
  bind_rows()

# Species with 2 adult stage classifications
spp_attr %>% filter(!is.na(adult_2))
# Species with two larval classifications
spp_attr %>% filter(!is.na(`larva > planula_2`))
# Do any species have both larva and `larva > planula`?
spp_attr %>% filter(!is.na(larva) & !is.na(`larva > planula`))
# Do any species have both polyp and adult?
spp_attr %>% filter(!is.na(polyp) & !is.na(adult))
# How many species have juvenile data?
spp_attr %>% filter(!is.na(juvenile))

# simplified - just adult and larval FGs
spp_attr_simple <- spp_attr %>%
  mutate(larva = ifelse(is.na(larva), `larva > planula`, larva),
         adult = ifelse(is.na(adult), polyp, adult)) %>% 
  select(AphiaID, adult, larva)

# get OBIS checklist for each
spp_obis <- checklist(taxonid = biotic_aphias_matched$AphiaID_accepted) %>% 
  as_tibble()

# Note - this returns AphiaIDs that are not in biotic Aphias:
spp_obis %>% filter(!(taxonID %in% biotic_aphias_matched$AphiaID_accepted))
# Check the taxonomic rank of these:
spp_obis %>% filter(!(taxonID %in% biotic_aphias_matched$AphiaID_accepted)) %>% count(taxonRank)
# Check total number of OBIS records:
spp_obis %>% filter(!(taxonID %in% biotic_aphias_matched$AphiaID_accepted)) %>% summarise(sum(records))
# Decision: strip these out of the dataset, and simplify to just OBIS records
spp_obis_simple <- spp_obis %>%
  filter(taxonID %in% biotic_aphias_matched$AphiaID_accepted) %>% 
  select(taxonID, records) %>% 
  rename(AphiaID = taxonID, obis_records = records)

# join obis and FG
spp_fg_obis <- full_join(spp_attr_simple, spp_obis_simple, by = "AphiaID")

# add taxonomic info back into substratum data, then add FG and OBIS#
substratum_full <- substratum_wide %>% left_join(
  select(biotic_aphias_matched, name, AphiaID, AphiaID_accepted, ScientificName_accepted, Kingdom:Family),
  by = c("SpeciesName" = "name")) %>% 
  filter(Kingdom == "Animalia") %>% 
  select(-c(Kingdom)) %>% 
  left_join(spp_fg_obis, by = c("AphiaID_accepted" = "AphiaID")) %>% 
  select(AphiaID_accepted, ScientificName_accepted, adult, larva, obis_records, everything())

# check adult functional groups
substratum_full %>% count(adult)

# restrict to benthos (broad sense)
substratum_full <- substratum_full %>%
  filter(adult %in% c("benthos", "endobenthos", "epibenthos", "macrobenthos"))

# Add in other traits
# Create a simple version of env_pos
# There are quite a few classifications here but most can be simplified to in/epi faun
env_pos %>% count(envpos)

env_pos_simple <- env_pos %>% 
  left_join(select(biotic_aphias_matched, name, AphiaID_accepted), by = c("SpeciesName" = "name")) %>% 
  filter(AphiaID_accepted %in% substratum_full$AphiaID_accepted) %>% 
  mutate(envpos_simple = ifelse(str_detect(envpos, "Epi"), "Epifaunal", envpos)) %>% 
  count(AphiaID_accepted, envpos_simple) %>%
  mutate(n = 1) %>% 
  pivot_wider(names_from = envpos_simple, values_from = n) %>% 
  rowwise() %>% 
  mutate(n_pos = sum(Epifaunal, Infaunal, Demersal, Interstitial, Lithotomous, na.rm = TRUE)) %>% 
  select(AphiaID_accepted, n_pos, everything()) %>% 
  ungroup()

# Check species with multiple classifications
env_pos_simple %>% filter(n_pos > 1)
# For all of these we can use infaunal / epifaunal or 'both'
# Check species with only one value, in one of Demersal, Interstitial, Lithotomous:
env_pos_simple %>% filter(n_pos == 1 & (Demersal == 1 | Interstitial == 1 | Lithotomous == 1))

# 107387 is _Liocarcinus depurator_, swimming crab, listed in Biotic as swimmer/crawler/burrower so inf/epi seems appropriate
# 138945 is _Caecum armoricum_, a gastropod, interstitial -> infauna
# 140770 is _Pholas dactylus_, a burrowing mollusc (common piddock), which burrows into soft rock / artificial structures. Hard substrate (bedrock) so classing here as epifauna
#Update these, and simplify to infauna/epifauna/both
env_pos_simple <- env_pos_simple %>%
  mutate(Epifaunal = ifelse(AphiaID_accepted %in% c(107387, 138945), 1, Epifaunal),
                          Infaunal = ifelse(AphiaID_accepted %in% c(107387, 140770), 1, Infaunal),
                          inf_epi = case_when(
                            Epifaunal == 1 & Infaunal == 1 ~ "Both",
                            Epifaunal == 1 & is.na(Infaunal) ~ "Epifaunal",
                            Infaunal == 1 & is.na(Epifaunal) ~ "Infaunal"
                          )
                          ) %>% 
  select(AphiaID_accepted, inf_epi)

# add to substratum data
substratum_full <- substratum_full %>% 
  left_join(env_pos_simple, by = "AphiaID_accepted") %>% 
  select(AphiaID_accepted:obis_records, inf_epi, everything())


# Create a simple version of dev_mech
# There are quite a few classifications here
dev_mech %>% count(devmech)

# Restrict to species in substratum dataset, and simplify dev mech where possible. NB - viviparity / oviparity tells us nothing about larval dispersal (e.g. Bugula turbinata is listed as viviparous, but also has lecithotrophic larvae - see http://www.marlin.ac.uk/biotic/browse.php?sp=4418)
dev_mech_simple <- dev_mech %>% 
  left_join(select(biotic_aphias_matched, name, AphiaID_accepted), by = c("SpeciesName" = "name")) %>% 
  filter(AphiaID_accepted %in% substratum_full$AphiaID_accepted) %>% 
  mutate(devmech_simple = case_when(
    devmech %in% c("Brooding", "Direct Development", "Schizotomous") ~ "Direct",
    devmech %in% c("Lecithotrophic", "Planktotrophic") ~ "Planktonic",
    TRUE ~ "Other"
  )) %>% 
  count(AphiaID_accepted, devmech_simple) %>%
  mutate(n = 1) %>% 
  pivot_wider(names_from = devmech_simple, values_from = n) %>% 
  rowwise() %>% 
  mutate(n_devmech = sum(Planktonic, Direct, Other, na.rm = TRUE)) %>% 
  select(AphiaID_accepted, n_devmech, everything()) %>% 
  ungroup()

# Check species with planktonic and direct classifications
dev_mech_simple %>% filter(Planktonic == 1 & Direct == 1)
# This includes things like _Cordylophora caspia_ which have both sexual (planktonic) and asexual (direct) reproduction. For our purposes - what's important is if larvae *might* turn up in a plankton survey, so we want a planktonic yes/no/unknown variable only, and can just join that to substratum data
dev_mech_simple <- dev_mech_simple %>%
  mutate(plank_biotic = case_when(
  Planktonic == 1 ~ "Yes",
  Direct == 1 & is.na(Planktonic) ~ "No",
  TRUE ~ "Unknown"))

# add to substratum data
substratum_full <- substratum_full %>% 
  left_join(select(dev_mech_simple, AphiaID_accepted, plank_biotic), by = "AphiaID_accepted") %>% 
  mutate(plank_larv = case_when(
    larva == "zooplankton" | plank_biotic == "Yes" ~ "Yes",
    plank_biotic == "No" ~ "No",
    TRUE ~ "Unknown")) %>% 
  select(AphiaID_accepted, ScientificName_accepted, obis_records, inf_epi, plank_larv,
         Bedrock:Strandline, everything())

# dig in to this a bit - useful to know which columns have very little data:
miss_var_summary(substratum_full) %>% data.frame()
substratum_full %>% filter(Pelagic == 1) %>% data.frame()

# some tidying to remove/rename columns
substratum_full <- substratum_full %>% select(-c(adult, larva, plank_biotic)) %>% 
  rename(biotic_SpeciesName = SpeciesName, biotic_AphiaID = AphiaID)

# Now export this derived dataset
write_csv(substratum_full, "data/derived_data/benthic_species_substratum_prefs.csv")
# and the substrate values table
write_csv(substrate_values, "data/derived_data/substrate_values_key.csv")
