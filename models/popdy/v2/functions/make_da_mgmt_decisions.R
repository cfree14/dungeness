
# sample_results <- da_sample_results_all; delay_thresh <- 3; reopen_thresh <- 1
make_da_mgmt_decisions <- function(sample_results, delay_thresh, reopen_thresh){
  
  # Inspect the first sample collected at each site and decide whether to continue monitoring
  da_sample_results_first <- sample_results %>% 
    # Reduce to first sample at each site
    filter(sample_num==1) %>% 
    # Determine clean or contaminated (against delay threshold)
    mutate(results=ifelse(nover >= delay_thresh, "contaminated", "clean")) %>% 
    # Summarize by DA management zone (area) 
    group_by(area) %>% 
    summarize(nsites_contam=sum(results=="contaminated"),
              results=ifelse(any(results=="contaminated"), "contaminated", "clean")) %>% 
    ungroup()
  
  # Identify which sites (both in a contaminated area) to continue to monitor
  areas_to_monitor <- da_sample_results_first %>% 
    filter(results=="contaminated") %>% pull(area) %>% as.character()
  
  # Continue monitoring if necessary
  if(length(areas_to_monitor)>0){
  
    # Decide when to stop monitoring and re-open the fishery
    da_mgmt_decisions <- sample_results %>% 
      # Reduce to only areas that are monitored after the first sample
      filter(area %in% areas_to_monitor) %>% 
      # Determine clean or contaminated (against re-open threshold)
      mutate(results=ifelse(sample_num==1 | nover > reopen_thresh, "contaminated", "clean")) %>% 
      # Summarize by DA management zone (area) and week
      group_by(area, sample_num, sample_week, sample_date) %>% 
      summarize(novers=paste(nover, collapse=", "),
                nsites_contam=sum(results=="contaminated"),
                results=ifelse(any(results=="contaminated"), "contaminated", "clean")) %>% 
      ungroup() %>% 
      # Arrange
      arrange(area, sample_num) %>% 
      # Add number of consecutive clean tests
      group_by(area) %>% 
      mutate(n_clean_tests=zoo::rollapply(results, width=2, align="right", FUN=function(x) sum(x=="clean"), fill=0)) %>% 
      # Identify the first week with two consecutive clean tests
      mutate(first_sample_2clean = min(which(n_clean_tests==2))) %>% 
      # Add fishery status
      mutate(da_mgmt_status=ifelse(sample_num<=(first_sample_2clean+1), "closed", "open")) %>% 
      ungroup()
    
    # Extract the sample results that were used to make these decisions
    results_used_key <- da_mgmt_decisions %>% 
      # Reduce to useful columns
      select(area, sample_num, da_mgmt_status) %>% 
      # Reduce to only surveys that were used
      # (i.e., a survey conducted while closed, except the last one)
      filter(da_mgmt_status=="closed") %>% 
      group_by(area) %>% 
      slice(1:(n()-1)) %>% 
      ungroup() %>% 
      # Indicated used
      select(-da_mgmt_status) %>% 
      mutate(used=T)
    
    # Determine which sample results were actually used
    sample_results_used <- sample_results %>% 
      # Add used key
      left_join(results_used_key, by=c("area", "sample_num")) %>% 
      # Reduce to samples used
      filter(sample_num==1 | used==T) %>% 
      select(-used) %>% 
      # Add status
      mutate(status=ifelse((sample_num==1 & nover >= delay_thresh) | (sample_num>1 & nover > reopen_thresh), "contaminated", "clean")) %>% 
      # Eliminate samples collected after season ends
      # Add region, add season close date, and trim surveys
      left_join(blocks_key %>% select(block_id, block_region), by="block_id") %>% 
      mutate(last_week=recode(block_region, 'Central/Southern'=40, "Northern"=42) %>% as.numeric()) %>% 
      filter(sample_week <= last_week) %>% 
      select(-c(block_region, last_week))
    
  }else{
    
    # No management decisions necessary
    da_mgmt_decisions <- NULL
    
    # Only first samples occur
    sample_results_used <- sample_results %>% 
      filter(sample_num==1) %>% 
      mutate(status="clean")
    
  }

  # Return
  output <- list(mgmt_decisions=da_mgmt_decisions, survey_results=sample_results_used)
  return(output)
  
}
