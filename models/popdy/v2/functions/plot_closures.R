

plot_closures <- function(output, zone="ramp", plot_name=NULL){
  
  # Split output
  results <- output$results
  params <- output$parameters
  entanglements <- output$entanglements
  
  # Zone to group by
  if(zone=="da"){zone_col <- "block_dzone"}
  if(zone=="ramp"){zone_col <- "block_ramp"}
  
  # Format DA survey results
  if(!is.null(output$da_survey_results)){
  da_survey_results <- output$da_survey_results %>% 
    # Rename columns for plotting
    rename(week=sample_week, block_dzone=area) %>% 
    # Make block ids character for plotting
    mutate(block_id=as.character(block_id)) %>% 
    # Rename zone column for plotting
    rename(zone=zone_col)
    
  }
  
  # Format entanglements data
  entanglements_df <- entanglements %>% 
    # Add number
    mutate(num=1:n(),
           block_id=as.character(block_id)) %>% 
    # Useful columns
    select(num, block_ramp, block_dzone, block_id, week_entangled, week_observed) %>% 
    # Gather
    gather(key="type", value="week", 5:ncol(.)) %>% 
    mutate(type=recode_factor(type, "week_entangled"="Entangled", "week_observed"="Observed")) %>% 
    # Rename zone column for plotting
    rename(zone=zone_col)
  
  # Format data for plotting
  pdata <- results %>% 
    rename(zone=zone_col) %>% 
    mutate(block_id=as.character(block_id),
           closure=factor(closure, levels=c("Out-of-season",
                                            "Season open",
                                            "DA closure",
                                            "April 1 closure",
                                            "Entanglement closure",
                                            "Marine life concentration closure")))
  
  # Build plot title:
  mgmt_text <- paste0("Management: ", params$management)
  if(params$effort_dynamics!="biomass-coupled"){
    effort_text <- paste0("Effort dynamics: ", params$effort_dynamics)
  }else{
    effort_text <- paste0("Effort dynamics: ", params$effort_dynamics, " (a=", params$a, ", b=", params$b, ")")
  }
  title_text <- paste(mgmt_text, effort_text, sep="\n")
  
  # Plot data
  g <- ggplot(pdata, aes(x=week, y=reorder(block_id, desc(block_id)), fill=closure)) +
    # Plot closure raster
    geom_raster() +
    facet_grid(zone~season, scale="free_y", space="free_y") +
    # Plot DA survey results (if available)
    {if(!is.null(output$da_survey_results)){geom_point(data=da_survey_results, mapping=aes(x=week, y=block_id, shape=status), inherit.aes = F)}} +
    # Plot whale entanglements
    geom_line(data=entanglements_df, mapping=aes(x=week, y=block_id, group=num), inherit.aes=F) +
    geom_point(data=entanglements_df, mapping=aes(x=week, y=block_id, color=type), inherit.aes = F, size=2) +
    # Labels
    labs(x="Model week", y="Block", title=title_text) +
    # Legends
    scale_fill_manual(name="", values=c("grey90", "grey50", "pink", "navy", "darkred", "darkgreen"), drop=F) +
    scale_shape_manual(name="", values=c(1, 16)) +
    scale_color_manual(name="", values=c("navy", "lightblue")) +
    # Theme
    theme_bw() + 
    theme(legend.position="bottom",
          axis.text.y=element_text(size=6),
          legend.title=element_blank())
  print(g)
  
  # Export plot
  if(!is.null(plot_name)){
    ggsave(g, filename=file.path(plotdir, paste0(plot_name, ".png")), 
           width=6.5, height=8, units="in", dpi=600)
  }

  
}




