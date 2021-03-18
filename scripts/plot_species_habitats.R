plot_species_habitats <- function(sp_id,
                                  event_data = benth_events,
                                  abundance_data = benth_abundances,
                                  habitat_trait_data = benth_substrate_prefs,
                                  habitat_trait_defs = benth_substrate_key,
                                  print_plot = TRUE, save_plot = FALSE,
                                  replace_plot = FALSE){
  # function to plot habitat affinities for a benthic species identified by its aphia ID
  
  # requires the following packages to be installed and loaded
  require(tidyverse)
  require(ggridges)
  require(ggtext)
  require(worrms)
  require(patchwork)

  # first check if plot exists and exit if it does, and if printing is not reuqired
  if(save_plot == TRUE){
    # create filename, and directory if needed
    plot_fname <- paste0("Aphia", sp_id, "_habitat_plot.pdf")
    if(!dir.exists(here::here("product", "species_hab_plots"))){
      invisible(dir.create(here::here("product", "species_hab_plots")))
      }
    if(replace_plot == FALSE){
      # check if file exists
      plot_exists <- file.exists(
        here::here("product/species_hab_plots", plot_fname))
      if(plot_exists == TRUE & print_plot == TRUE){save_plot <- FALSE}
      if(plot_exists == TRUE & print_plot == FALSE){return(NULL)}
    }
    }
  
    
  
  # filter abundance dataset to species ID
  abundance_data <- abundance_data %>% filter(AphiaID == sp_id)
  
  # add abundance data to event dataset and filter to events species occurs in
  sp_event_data <- abundance_data %>%
    left_join(event_data, by = "eventNummer")
  
  
  # NB - a few events have multiple habitat entries.
  # For these, divide the abundance of the species equally between instances of the event
  event_freqs <- sp_event_data %>% count(eventNummer)
  sp_event_data <- sp_event_data %>%
    left_join(event_freqs, by = "eventNummer") %>% 
    rename(no_classifications = n) %>% 
    mutate(abundance_stand = abundance / no_classifications,
           abundance_stand1 = abundance_stand / sum(abundance_stand))
  
  
  # add variable indicating presence
  sp_event_data <- sp_event_data %>% mutate(presence = "present")
  
  
  # Do same for full event data
  # Set presence to 'all events' and abundance = 1
  # Then divide abundance between instancees of same event
  event_data <- event_data %>% mutate(presence = "all events", abundance = 1)
  event_freqs <- event_data %>% count(eventNummer)
  event_data <- event_data %>%
    left_join(event_freqs, by = "eventNummer") %>% 
    rename(no_classifications = n) %>% 
    mutate(abundance_stand = abundance / no_classifications,
           abundance_stand1 = abundance_stand / sum(abundance_stand))
  
  # now join the two data frames
  sp_event_data <- bind_rows(sp_event_data, event_data)
  msfd_vals <- unique(as.vector(event_data$MSFD_BBHT))
  msfd_levels <- c("Abyssal", "Lower bathyal sediment",
                   str_subset(msfd_vals, "Upper"), 
                   str_subset(msfd_vals, "Offshore"),
                   str_subset(msfd_vals, "Circalittoral"),
                   str_subset(msfd_vals, "Infralittoral"))
  sp_event_data <- sp_event_data %>% 
    mutate(presence = fct_relevel(presence, "present", "all events"),
           Substrate = fct_relevel(str_trim(Substrate, side = "both"),
                                   "Seabed", "Rock or other hard substrata",
                                   "Coarse substrate", "Sand", "Sandy mud", "Muddy sand",
                                   "Sandy mud or Muddy sand",
                                   "Fine mud or Sandy mud or Muddy sand", "Fine mud",
                                   "Mixed sediment", "Sediment"),
           EUNIScomb = as.vector(EUNIScomb),
           EUNIScomb = case_when(
             EUNIScomb == "Na" ~ "NA",
             EUNIScomb == " " ~ "NA",
             TRUE ~ EUNIScomb),
           MSFD_BBHT = as.vector(MSFD_BBHT),
           MSFD_BBHT = ifelse(MSFD_BBHT == "Na", NA, MSFD_BBHT),
           MSFD_BBHT = fct_relevel(MSFD_BBHT, msfd_levels)
    )
  
  #### Continuous variable ridge plots
  
  ## log_D50
  # create a dataframe with no missing values, and re-calculate abundance weighting variable
  g_data <- sp_event_data %>% filter(!is.na(log_D50))
  g_data$abundance_stand1[g_data$presence == "present"] <- 
    g_data$abundance_stand[g_data$presence == "present"] /
    sum(g_data$abundance_stand[g_data$presence == "present"])
  g_data$abundance_stand1[g_data$presence != "present"] <- 
    g_data$abundance_stand[g_data$presence != "present"] /
    sum(g_data$abundance_stand[g_data$presence != "present"])
  
  if(sum(g_data$presence == "present") > 2){
    
    # produce the plot
    d50_plot <- ggplot(g_data, aes(x = log_D50, y = presence)) +
      geom_density_ridges(aes(height = ..density..,
                              weight = abundance_stand1,
                              fill = presence),
                          alpha = 0.5, show.legend = FALSE, scale = 5,
                          stat = "density") +
      scale_fill_manual(values = c("darkorange", "grey")) +
      ylab(element_blank()) +
      xlab("") +
      theme_ridges() +
      ggtitle("log(D50)")
  } else {
    d50_plot <- ggplot(filter(g_data, presence != "present"),
                       aes(x = log_D50)) +
      geom_density(alpha = 0.5, show.legend = FALSE, fill = "grey") +
      ylab(element_blank()) +
      xlab(element_blank()) +
      theme_ridges() +
      theme(axis.text.y = element_blank()) +
      ggtitle("log(D50)")
  }
  ## Mud%
  # create a dataframe with no missing values, and re-calculate abundance weighting variable
  g_data <- sp_event_data %>% filter(!is.na(MudPercent))
  g_data$abundance_stand1[g_data$presence == "present"] <- 
    g_data$abundance_stand[g_data$presence == "present"] /
    sum(g_data$abundance_stand[g_data$presence == "present"])
  g_data$abundance_stand1[g_data$presence != "present"] <- 
    g_data$abundance_stand[g_data$presence != "present"] /
    sum(g_data$abundance_stand[g_data$presence != "present"])
  
  # produce the plot
  if(sum(g_data$presence == "present") > 2){
    mud_plot <- ggplot(g_data, aes(x = MudPercent, y = presence)) +
      geom_density_ridges(aes(height = ..density..,
                              weight = abundance_stand1,
                              fill = presence),
                          alpha = 0.5, show.legend = FALSE, scale = 5,
                          stat = "density") +
      scale_fill_manual(values = c("darkorange", "grey")) +
      ylab(element_blank()) +
      xlab(element_blank()) +
      ggtitle("Mud %") +
      theme_ridges()
  } else {
    mud_plot <- ggplot(filter(g_data, presence != "present"),
                       aes(x = MudPercent)) +
      geom_density(alpha = 0.5, show.legend = FALSE, fill = "grey") +
      ylab(element_blank()) +
      xlab(element_blank()) +
      theme_ridges() +
      theme(axis.text.y = element_blank()) +
      ggtitle("Mud %")    
  }  
  ## Sand%
  # create a dataframe with no missing values, and re-calculate abundance weighting variable
  g_data <- sp_event_data %>% filter(!is.na(SandPercent))
  g_data$abundance_stand1[g_data$presence == "present"] <- 
    g_data$abundance_stand[g_data$presence == "present"] /
    sum(g_data$abundance_stand[g_data$presence == "present"])
  g_data$abundance_stand1[g_data$presence != "present"] <- 
    g_data$abundance_stand[g_data$presence != "present"] /
    sum(g_data$abundance_stand[g_data$presence != "present"])
  
  # produce the plot
  if(sum(g_data$presence == "present") > 2){
    sand_plot <- ggplot(g_data, aes(x = SandPercent, y = presence)) +
      geom_density_ridges(aes(height = ..density..,
                              weight = abundance_stand1,
                              fill = presence),
                          alpha = 0.5, show.legend = FALSE, scale = 5,
                          stat = "density") +
      scale_fill_manual(values = c("darkorange", "grey")) +
      ylab(element_blank()) +
      xlab(element_blank()) +
      ggtitle("Sand %") +
      theme_ridges()
  } else {
    sand_plot <- ggplot(filter(g_data, presence != "present"),
                        aes(x = SandPercent)) +
      geom_density(alpha = 0.5, show.legend = FALSE, fill = "grey") +
      ylab(element_blank()) +
      xlab(element_blank()) +
      theme_ridges() +
      theme(axis.text.y = element_blank()) +
      ggtitle("Sand %")    
  }
  ## Gravel%
  # create a dataframe with no missing values, and re-calculate abundance weighting variable
  g_data <- sp_event_data %>% filter(!is.na(GravelPercent))
  g_data$abundance_stand1[g_data$presence == "present"] <- 
    g_data$abundance_stand[g_data$presence == "present"] /
    sum(g_data$abundance_stand[g_data$presence == "present"])
  g_data$abundance_stand1[g_data$presence != "present"] <- 
    g_data$abundance_stand[g_data$presence != "present"] /
    sum(g_data$abundance_stand[g_data$presence != "present"])
  
  # produce the plot
  if(sum(g_data$presence == "present") > 2){
    gravel_plot <- ggplot(g_data, aes(x = GravelPercent, y = presence)) +
      geom_density_ridges(aes(height = ..density..,
                              weight = abundance_stand1,
                              fill = presence),
                          alpha = 0.5, show.legend = FALSE, scale = 5,
                          stat = "density") +
      scale_fill_manual(values = c("darkorange", "grey")) +
      ylab(element_blank()) +
      xlab(element_blank()) +
      ggtitle("Gravel %)") +
      theme_ridges()
  } else {
    gravel_plot <- ggplot(filter(g_data, presence != "present"),
                          aes(x = GravelPercent)) +
      geom_density(alpha = 0.5, show.legend = FALSE, fill = "grey") +
      ylab(element_blank()) +
      xlab(element_blank()) +
      theme_ridges() +
      theme(axis.text.y = element_blank()) +
      ggtitle("Gravel %")    
  }
  #### Categorical variable frequency plots
  
  # Substrate
  substrate_plot <- ggplot(sp_event_data, aes(x = Substrate, y = ..prop.., group = 1)) +
    geom_bar(aes(fill = presence, weight = abundance_stand), alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "grey")) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~presence) +
    coord_flip() +
    theme_minimal() +
    xlab(element_blank()) +
    ylab("") +
    ggtitle("Substrate") +
    theme(legend.position = "none",
          plot.title.position = "panel",
          axis.text.x = element_text(angle = 45, size = 5))
  
  # EUNIS classification
  eunis_plot <- ggplot(sp_event_data, aes(x = EUNIScomb, y = ..prop.., group = 1)) +
    # lumping EUNIS to create 'other' category
    #ggplot(sp_event_data, aes(x = forcats::fct_lump(EUNIScomb, n = 6),
    #                         y = ..prop.., group = 1)) +
    geom_bar(aes(fill = presence, weight = abundance_stand), alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "grey")) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~presence) +
    coord_flip() +
    theme_minimal() +
    xlab(element_blank()) +
    ylab("") +
    ggtitle("EUNIS classification") +
    theme(legend.position = "none",
          plot.title.position = "panel",
          axis.text.x = element_text(angle = 45, size = 5),
          axis.text.y = element_text(size = 5))
  
  # MSFD
  msfd_plot <- ggplot(sp_event_data, aes(x = MSFD_BBHT, y = ..prop.., group = 1)) +
    geom_bar(aes(fill = presence, weight = abundance_stand), alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "grey")) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~presence) +
    coord_flip() +
    theme_minimal() +
    xlab(element_blank()) +
    ylab("") +
    ggtitle("MSFD classification") +
    theme(legend.position = "none",
          plot.title.position = "panel",
          axis.text.x = element_text(angle = 45, size = 5),
          axis.text.y = element_text(size = 8))
  
  # create plot title and subtitle
  # title is from species name and AphiaID
  taxon_name <- wm_id2name(sp_id)
  plot_tit <- paste0("*", taxon_name, "* (Aphia ID: ", sp_id, ")")
  
  # subtitle from no. events and Biotic habitat preferences, if available
  sp_event_data <- sp_event_data %>% filter(presence == "present")
  n_sedmatch <- sum(!is.na(sp_event_data$log_D50))
  n_habmatch <- sum(!is.na(sp_event_data$Substrate))
  plot_subtit <- paste0(n_sedmatch, " occurrences matched to sediment and ",
                        n_habmatch, " matched to habitat\n")
  
  hab_pref_dat <- habitat_trait_data %>% filter(AphiaID_accepted == sp_id)
  if(nrow(hab_pref_dat) == 0){
    plot_subtit <- paste0(plot_subtit, "No habitat preference data in Biotic")
  } else {
    hab_pref_text <- hab_pref_dat %>%
      dplyr::select(Bedrock:Strandline) %>% 
      pivot_longer(
        cols = everything(), names_to = "substrate", values_to = "substrate_pref") %>%
      filter(!is.na(substrate_pref)) %>% 
      left_join(habitat_trait_defs, by = c("substrate" = "substratum_code"))
    hab_pref_text <- paste(hab_pref_text$substratum, collapse = ", ")
    plot_subtit <- paste0(plot_subtit, "Habitat preference from Biotic: ", hab_pref_text)
  }
  
  species_plot <- (d50_plot | mud_plot | sand_plot | gravel_plot) /
    (substrate_plot | eunis_plot | msfd_plot )
  
  species_plot <- species_plot + plot_annotation(
    title = plot_tit,
    subtitle = plot_subtit,
    theme = theme(plot.title = ggtext::element_markdown())
  )
  
  if(print_plot == TRUE){print(species_plot)}
  if(save_plot == TRUE){
  
    ggsave(filename = paste0(here::here("product/species_hab_plots/"), plot_fname),
           plot = species_plot,
           height = 210, width = 297, units = "mm")
    
  }
  
}



            