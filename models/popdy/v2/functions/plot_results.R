
# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  strip.text=element_text(size=9),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot simulation results
# output <- data8; plot_obs <- F; plot_name <- "2015_effort_reduction_70%"
plot_results <- function(output, plot_obs=F, plot_name=NULL){
  
  # Split output
  results <- output$results
  params <- output$parameters
  
  # Summarize weekly stats
  stats <- results %>% 
    # Calculate weekly stats
    group_by(season, block_region, week) %>% 
    summarize_at(.vars=c("biomass_mt", "catch_mt", "traps_n", 
                         "whales_n", "encounters_n"), sum, na.rm=T) %>% 
    mutate(traps_n=traps_n/1000,
           encounters_n=encounters_n/1000) %>% 
    ungroup() %>% 
    # Convert wide to long
    gather(key="variable", value="value", 4:ncol(.)) %>% 
    # Rename variables
    mutate(variable=recode_factor(variable, 
                                  "biomass_mt"="Biomass (mt)",
                                  "catch_mt"="Catch (mt)",
                                  "traps_n"="# of traps\n(1000s)",
                                  "whales_n"="# of whales\nin fishing grounds",
                                  "encounters_n"="# of trap-whale\nencounters (1000s)"))
  
  # Entanglements
  entanglements <- results %>% 
    group_by(season, week) %>% 
    summarise(n_entangled=sum(entanglements_n, na.rm=T)) %>% 
    filter(n_entangled>0)
  
  # Build observations dataset
  # obs <- data_wk_avg %>% 
  #   select(-c(n_vessels, season)) %>% 
  #   rename(week=model_week) %>% 
  #   filter(week>=0) %>% 
  #   mutate(n_traps=n_traps/1000) %>% 
  #   gather(key="variable", value="value", 2:3) %>% 
  #   mutate(variable=recode_factor(variable, 
  #                          "n_traps"="Thousands of traps",
  #                          "landings_mt"="Catch (mt)"))
    
  # Build plot title:
  mgmt_text <- paste0("Management: ", params$management)
  if(params$effort_dynamics!="biomass-coupled"){
    effort_text <- paste0("Effort dynamics: ", params$effort_dynamics)
  }else{
    effort_text <- paste0("Effort dynamics: ", params$effort_dynamics, " (a=", params$a, ", b=", params$b, ")")
  }
  title_text <- paste(mgmt_text, effort_text, sep="\n")
  
  # Plot weekly stats
  alpha_value <- ifelse(plot_obs, 1, 0)
  g <- ggplot(stats, aes(x=week, y=value, fill=block_region)) +
    facet_grid(variable ~ season, scale="free_y") +
    geom_area() +
    # Add observations
    # geom_line(obs, mapping=aes(x=week, y=value, fill=NULL), color="black", alpha=alpha_value) +
    # Add entanglment lines
    geom_vline(entanglements, mapping=aes(xintercept=week), color="navyblue") +
    # Labels
    labs(x="Week", y="", title=title_text) +
    scale_fill_discrete(name="Region") +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "bottom")
  print(g)
  
  # Export plot
  if(!is.null(plot_name)){
    ggsave(g, filename=file.path(plotdir, paste0(plot_name, ".png")), 
           width=6.5, height=8, units="in", dpi=600)
  }
  
}
