

# Merge scenario results
merge_iterations <- function(scenario, indir, outdir){
  
  # Loop through results and merge
  # x <- files2merge[1]
  files2merge <- list.files(indir, pattern=scenario)
  results <- purrr::map_df(files2merge, function(x) {
    
    # Read data
    sdata <- readRDS(file.path(indir, x))
    iter <- gsub(paste0(scenario, "_"), "", x) %>% gsub(".Rds", "", .) %>% as.numeric()
    sdata1 <- sdata$results %>% 
      mutate(scenario=scenario,
             iteration=iter) %>% 
      select(scenario, iteration, everything())
    
  })
  
  # Export results
  outfile <- paste0(scenario, ".Rds")
  saveRDS(results, file.path(outdir, outfile))
  
  # Return results
  return(results)
  
}
