

# Merge scenario results
plot_scenario_comparison <- function(results){
  
  # Plot performance metrics
  g <- ggplot(results, aes(x=season, y=value, fill=scenario)) +
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
