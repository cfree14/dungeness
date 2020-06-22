

# Merge scenario results
plot_merged_results <- function(results){
  
  # Calculate performance metrics
  stats <- results %>% 
    # Calculate performance stats by scenario-iteractio
    group_by(scenario, season, iteration) %>% 
    summarize(catch_mt=sum(catch_mt),
              catch_mt_contam=sum(catch_mt_contam),
              pcontam=catch_mt_contam/catch_mt,
              entanglements=sum(entanglements_n)) %>% 
    ungroup() %>% 
    # Remove unnecessary columns
    select(-catch_mt_contam) %>% 
    # Convert wide-to-long for plotting
    gather(key="metric", value="value", 4:ncol(.)) %>% 
    # Format metric names
    mutate(metric=recode_factor(metric, 
                                "catch_mt"="Catch (mt)",
                                "pcontam"="Proportion of catch\ncontaminated with domoic acid",
                                "entanglements"="Number of humback\nwhale entanglements"))
  
  # Plot performance metrics
  g <- ggplot(stats, aes(x=season, y=value)) +
    geom_boxplot() +
    facet_wrap(~metric, scale="free_y") + 
    labs(x="Season", y="") +
    expand_limits(y=0) +
    theme_bw()
  g
  print(g)
  
  # Return
  return(stats)

}
