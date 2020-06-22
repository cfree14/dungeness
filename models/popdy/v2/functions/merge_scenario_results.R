

# Merge scenario results
merge_scenario_results <- function(scenarios){
  
  # Directories
  dir <- "output"

  # Loop through results and merge
  results <- purrr::map_df(scenarios, function(x) {
    
    # Read data
    infile <- paste0(x, ".Rds")
    sdata <- readRDS(file.path(dir, infile))

    # Summarize data
    stats <- sdata %>% 
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
    
  })
  
  # Export results
  saveRDS(results, file.path(outdir, "merged_scenario_results.Rds"))
  
  # Return results
  return(results)
  
}
