

plot_closures <- function(output, plot_name=NULL){
  
  # Split output
  results <- output$results
  params <- output$parameters
  
  # Format data for plotting
  pdata <- results %>% 
    mutate(block_id=as.character(block_id),
           closure=factor(closure, levels=c("Out-of-season",
                                            "Season open",
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
  g <- ggplot(pdata, aes(x=week, y=block_id, fill=closure)) +
    geom_raster() +
    facet_grid(block_ramp~season, scale="free_y", space="free_y") +
    # Labels
    labs(x="Model week", y="Block", title=title_text) +
    scale_fill_manual(name="", values=c("grey80", "grey40", "navy", "darkred", "darkgreen"), drop=F) +
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




