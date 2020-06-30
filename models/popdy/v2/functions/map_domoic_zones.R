
# Assign domoic acid management zones to blocks based on lat
# lats <- da_site_key$lat_dd; zones <- da_site_key$area
map_domoic_zones <- function(lats, zones){
  
  # Calculate mean latitude for paired sample sites
  zone_lat_df <- data.frame(zone=as.character(zones), lat_dd=lats, stringsAsFactors = F) %>% 
    group_by(zone) %>% 
    summarise(lat_dd=mean(lat_dd)) %>% 
    arrange(desc(lat_dd))
  
  # Calculate means between descending latitudes
  zone_lat_divs <- zoo::rollmean(zone_lat_df$lat_dd, k=2, align="right")
  
  # Label blocks by zone
  blocks_sf_zoned <- blocks_sf %>% 
    mutate(block_dzone=cut(block_lat_dd, breaks=c(Inf, zone_lat_divs, -Inf), labels = rev(unique(zone_lat_df$zone))),
           block_dzone=as.character(block_dzone),
           block_dzone=ifelse(block_type=="Offshore", NA, block_dzone),
           block_dzone=factor(block_dzone, levels=unique(zone_lat_df$zone)))
  
  # Plot DA management zones
  usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
  g <- ggplot() +
    # Plot blocks
    geom_sf(data=blocks_sf_zoned, mapping=aes(fill=block_dzone)) +
    # Plot land
    geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
    geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
    # Plot sampling latitudes
    geom_hline(yintercept=lats, linetype="dotted", lwd=0.5) +
    geom_hline(data=zone_lat_df, aes(yintercept=lat_dd), linetype="dashed", lwd=0.8) +
    geom_hline(yintercept=zone_lat_divs, linetype="solid", lwd=1.2) +
    # Crop map
    coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
    # Legend
    scale_fill_discrete(name="DA mgmt zone", na.value=NA) +
    # Little things
    labs(x="", y="") +
    theme_bw() +
    theme(legend.position = c(0.12, 0.12))
  print(g)
  
  # Return
  return(blocks_sf_zoned)
    
}
