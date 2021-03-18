get_species_habitats <- function(sp_id,
                                 event_data = benth_events,
                                 abundance_data = benth_abundances,
                                 habitat_trait_data = benth_substrate_prefs){

  # Function to get and summarise benthic habitat data from a species' AphiaID
  
  # filter abundance dataset to species ID
  abundance_data <- abundance_data %>% filter(AphiaID == sp_id)
  
  # add abundance data to event dataset and filter to events species occurs in
  sp_event_data <- abundance_data %>%
    left_join(event_data, by = "eventNummer")
  
  # NB - a few events have multiple habitat entries.
  # For thse, divide the abundance of the species equally between instances of the event
  event_freqs <- sp_event_data %>% count(eventNummer)
  sp_event_data <- sp_event_data %>%
    left_join(event_freqs, by = "eventNummer") %>% 
    rename(no_classifications = n) %>% 
    mutate(abundance_stand = abundance / no_classifications)
  
  # gather continous variables and get mean values
  sp_event_cont <- sp_event_data %>%
    dplyr::select(eventNummer, abundance_stand,
                  MudPercent:log_D50) %>% 
    pivot_longer(cols = MudPercent:log_D50,
                 names_to = "substrate_var",
                 values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    group_by(substrate_var) %>% 
    summarise(wt_mean_val = weighted.mean(value, abundance_stand, na.rm = TRUE)) %>% 
    pivot_wider(names_from = substrate_var, values_from = wt_mean_val)
  
  # gather categorical variables and get frequencies
  get_cat_freqs <- function(cat_name){
    cat_id <- rlang::ensym(cat_name)
    sp_event_data %>% count(!!cat_id, wt = abundance_stand, name = "freq") %>%
      mutate(freq = freq / sum(freq),
             cat_id = paste(cat_name, !!cat_id, sep = "_")) %>% 
      dplyr::select(-1) %>% 
      pivot_wider(names_from = cat_id, values_from = "freq")
  }
  
  energy <- get_cat_freqs("Energy")
  biozone <- get_cat_freqs("Biozone")
  substrate <- get_cat_freqs("Substrate")
  salinity <- get_cat_freqs("Salinity")
  oxygen <- get_cat_freqs("Oxygen")
  EUNIScomb <- get_cat_freqs("EUNIScomb")
  MSFD_BBHT <- get_cat_freqs("MSFD_BBHT")
  
  sp_event_cat <- bind_cols(energy, biozone, substrate,
                            salinity, oxygen, EUNIScomb, MSFD_BBHT)
  
  # add some basic species summary stuff (Aphia ID, total number of events the species occurs in and its total and average abundance) and then compile:
  sp_summary <- sp_event_data %>% dplyr::select(eventNummer, abundance) %>% 
    distinct() %>% 
    summarise(total_occ = n(), total_ab = sum(abundance), mean_ab = mean(abundance)) %>% 
    mutate(AphiaID = sp_id) %>% 
    dplyr::select(AphiaID, everything()) %>% 
    bind_cols(sp_event_cont) %>% 
    bind_cols(sp_event_cat)
  
  # check if species is in habitat trait dataset
  if(sp_id %in% habitat_trait_data$AphiaID_accepted){
    habitat_trait_data <- habitat_trait_data %>%
      filter(AphiaID_accepted == sp_id) %>% 
      dplyr::select(inf_epi:Strandline)
    sp_summary <- sp_summary %>% bind_cols(habitat_trait_data)
  }
  
  sp_summary
  
}