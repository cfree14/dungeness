

# Plot simulation results
plot_results <- function(data){
  
  # 1. Total population stats
  #############################################
  
  # Total population stats
  tot_stats <- data %>% 
    group_by(step) %>% 
    summarize(q_total_mt=sum(biomass_mt),
              q_legal_mt=sum(biomass_mt[sex=="males" & age_yr>=4])) %>% 
    gather(key="type", value="biomass_mt", 2:ncol(.)) %>% 
    mutate(type=recode(type, "q_total_mt"="Total", "q_legal_mt"="Legal"))
  
  # Plot total population stats
  g1 <- ggplot(tot_stats, aes(x=step, y=biomass_mt, color=type)) +
    geom_line() +
    geom_hline(yintercept = legal_male_pop_est_mt) +
    labs(x="Week", y="Biomass (mt)", title="Abundance over time") +
    expand_limits(y = 0) +
    theme_bw()
  print(g1)
  
  
  # 2. Catch time series
  #############################################
  
  # Catch stats
  catch_stats <- data %>% 
    group_by(step) %>% 
    summarize(catch_n=sum(catch_n),
              catch_mt=sum(catch_mt))
  
  
  # Plot total population stats
  g2 <- ggplot(catch_stats, aes(x=step, y=catch_mt)) +
    geom_line() +
    labs(x="Week", y="Catch (mt)", title="Catch over time") +
    expand_limits(y = 0) +
    theme_bw()
  print(g2)
  
  # 3. Terminal year population structure
  #############################################
  
  last_year_stats <- data %>% 
    filter(step==max(step)) %>% 
    group_by(age_yr, sex) %>% 
    summarize(biomass_n=sum(biomass_n))
  
  g3 <- ggplot(last_year_stats, aes(x=age_yr, y=biomass_n/1e6, fill=sex)) +
    geom_bar(stat="identity") +
    labs(x="Age (yr)", y="Number of crabs (millions)", title="Age structure in final time step") +
    scale_x_continuous(breaks=1:max(data$age_yr)) +
    theme_bw()
  print(g3)
  
  #

}
