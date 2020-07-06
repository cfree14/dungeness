

plot_closures <- function(output, zone="ramp", plot_name=NULL){
  
  # Split output
  results <- output$results
  params <- output$parameters
  entanglements <- output$entanglements
  da_surveys <- output$da_survey_results
  max_traps <- output$max_traps
  
  # Zone to group by
  if(zone=="da"){zone_col <- "block_dzone"}
  if(zone=="ramp"){zone_col <- "block_ramp"}
  
  # Format entanglements data
  entanglements_df <- entanglements %>% 
    # Add number by season
    group_by(season) %>% 
    mutate(num=1:n()) %>% 
    ungroup() %>% 
    # Format blocks for plotting
    mutate(block_id=as.character(block_id),
           block_dzone=factor(block_dzone, levels=levels(results$block_dzone))) %>% 
    # Useful columns
    select(season, num, block_ramp, block_dzone, block_id, week_entangled, week_observed) %>% 
    # Gather
    gather(key="type", value="week", 6:ncol(.)) %>% 
    mutate(type=recode_factor(type, "week_entangled"="Entangled", "week_observed"="Observed")) %>% 
    arrange(season, num) %>% 
    # Rename zone column for plotting
    rename(zone=zone_col)
  
  # Format DA survey results
  da_surveyed <- ifelse("current domoic" %in% params$management, T, F)
  if(da_surveyed){
    da_survey_results <- da_surveys %>% 
      # Rename columns for plotting
      rename(week=sample_week, block_dzone=area) %>% 
      # Make block ids character for plotting
      mutate(block_id=as.character(block_id)) %>% 
      # Correct area factor
      mutate(block_dzone=factor(block_dzone, levels=levels(results$block_dzone))) %>% 
      # Rename zone column for plotting
      rename(zone=zone_col)
  }
  
  # Identify gear reduction events
  gear_red_events <- max_traps %>% 
    group_by(season) %>% 
    mutate(reduction_yn=zoo::rollapply(ntraps_max, width=2, FUN=sd, fill=0, align="right")!=0) %>% 
    filter(reduction_yn)
  
  # Format data for plotting
  pdata <- results %>% 
    rename(zone=zone_col) %>% 
    mutate(block_id=as.character(block_id),
           closure=factor(closure, levels=c("Out-of-season",
                                            "Season open",
                                            "DA closure",
                                            "Early closure",
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
    {if(da_surveyed){geom_point(data=da_survey_results, mapping=aes(x=week, y=block_id, shape=status), inherit.aes = F)}} +
    # Plot whale entanglements
    geom_line(data=entanglements_df, mapping=aes(x=week, y=block_id, group=num), inherit.aes=F) +
    geom_point(data=entanglements_df, mapping=aes(x=week, y=block_id, color=type), inherit.aes = F, size=2) +
    # Plot vertical line for gear reduction (if it happens)
    geom_vline(data=gear_red_events, mapping=aes(xintercept=week), linetype="dotted") +
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




