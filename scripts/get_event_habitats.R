get_event_habitats <- function(event_data = benth_events){
  # function to get benthic habitat summary stats for all sampling events
  
  # gather continous variables and get mean values
  event_cont <- event_data %>%
    dplyr::select(eventNummer,
                  MudPercent:log_D50) %>% 
    pivot_longer(cols = MudPercent:log_D50,
                 names_to = "substrate_var",
                 values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    group_by(substrate_var) %>% 
    summarise(mean_val = mean(value, na.rm = TRUE)) %>% 
    pivot_wider(names_from = substrate_var, values_from = mean_val)
  
  # gather categorical variables and get frequencies
  get_cat_freqs <- function(cat_name){
    cat_id <- rlang::ensym(cat_name)
    event_data %>% count(!!cat_id, name = "freq") %>%
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
  
  event_cat <- bind_cols(energy, biozone, substrate,
                         salinity, oxygen, EUNIScomb, MSFD_BBHT)
  
  # add some basic summary stuff and then compile
  event_summary <- event_data %>% dplyr::select(eventNummer) %>% 
    distinct() %>% 
    summarise(total_events = n()) %>% 
    bind_cols(event_cont) %>% 
    bind_cols(event_cat)
  
  event_summary
  
}