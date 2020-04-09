

# Plot simulation results
plot_results <- function(data){
  
  # Summarize weekly stats
  stats <- data %>% 
    # Calculate weekly stats
    group_by(block_region, week) %>% 
    summarize_at(.vars=c("biomass_mt", "catch_mt", "traps_n"), sum, na.rm=T) %>% 
    mutate(traps_n=traps_n/1000) %>% 
    ungroup() %>% 
    # Convert wide to long
    gather(key="variable", value="value", 3:ncol(.)) %>% 
    # Rename variables
    mutate(variable=recode_factor(variable, 
                                  "biomass_mt"="Biomass (mt)",
                                  "catch_mt"="Catch (mt)",
                                  "traps_n"="Thousands of traps"))

  # Plot weekly stats
  g <- ggplot(stats, aes(x=week, y=value, fill=block_region)) +
    facet_wrap(~variable, ncol=1, scale="free_y") +
    geom_area() +
    labs(x="Week", y="") +
    scale_fill_discrete(name="Region") +
    theme_bw() +
    theme(legend.position = "bottom")
  print(g)

}
