

# Plot simulation results
plot_results <- function(data){
  
  # Setup theme
  my_theme <- theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=12),
                    plot.title=element_text(size=14),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.position = "bottom",
                    legend.text=element_text(size=12),
                    legend.title = element_blank())
  
  
  # 1. Total population stats
  #############################################
  
  # Total population stats
  tot_stats <- data %>% 
    group_by(step) %>% 
    summarize(q_total_mt=sum(biomass_mt),
              q_legal_mt=sum(biomass_mt[sex=="males" & age_yr>=4])) %>% 
    gather(key="type", value="biomass_mt", 2:ncol(.)) %>% 
    mutate(type=recode(type, "q_total_mt"="All crabs", "q_legal_mt"="Legal-sized males"))
  
  # Plot total population stats
  g1 <- ggplot(tot_stats, aes(x=step, y=biomass_mt/1000, color=type)) +
    geom_line() +
    geom_hline(yintercept = legal_male_pop_est_mt/1000) +
    labs(x="Week", y="Biomass (1000s mt)", title="Abundance over time") +
    expand_limits(y = 0) +
    theme_bw() + my_theme
  print(g1)
  
  # 2. Catch time series
  #############################################
  
  # Catch time series if available
  if(sum(data$catch_mt, na.rm=T) > 0){
    
    # Format average cumulative catch to match simulated catch
    week1 <- min(data$step[!is.na(data$catch_mt) & data$catch_mt>0])
    avg_cum_catch1 <- avg_cum_catch %>% 
      rename(step=season_week, catch_mt_cum_prop_obs=landings_mt_cum_prop) %>% 
      filter(step>=0) %>% 
      mutate(step=step+week1)
  
    # Catch stats
    catch_stats <- data %>% 
      group_by(step) %>% 
      # Calcule total catch by time step
      summarize(catch_n=sum(catch_n),
                catch_mt=sum(catch_mt)) %>% 
      # Remove NA catch in first time step
      filter(!is.na(catch_mt)) %>% 
      # Calculate cumulative catch and proportion of cumulative catch
      mutate(catch_mt_cum=cumsum(catch_mt),
             catch_mt_cum_prop=catch_mt_cum/max(catch_mt_cum)) %>% 
      # Add observed cumulative catch
      left_join(avg_cum_catch1, by="step") %>% 
      mutate(catch_mt_cum_prop_obs=ifelse(is.na(catch_mt_cum_prop_obs) & step <10, 0, catch_mt_cum_prop_obs))
    
    # Nominal catch
    #############################
    
    # Total catch
    catch_tot_mt <- sum(catch_stats$catch_mt, na.rm=T)
    catch_tot_mt_format <- paste(format(catch_tot_mt, digits=0, big.mark = ","), "mt")
    preseason_tot_mt <- tot_stats$biomass_mt[tot_stats$step==1 & tot_stats$type=="Legal-sized males"]
    percent_caught <- catch_tot_mt / preseason_tot_mt * 100
    percent_caught_format <- paste0(round(percent_caught, digits = 1), "% caught")
    catch_label <- paste(catch_tot_mt_format, percent_caught_format, sep="\n")
  
    # Plot total catch time series
    g2 <- ggplot(catch_stats, aes(x=step, y=catch_mt/1000)) +
      geom_line() +
      labs(x="Week", y="Catch (1000s mt)", title="Catch over time") +
      expand_limits(y = 0) +
      annotate("text", label=catch_label, x=50, y=6, hjust=1, size=4) +
      theme_bw() + my_theme
    print(g2)
    
    # Cumulative catch
    #############################
    
    # Format for this plot
    catch_stats1 <- catch_stats %>% 
      select(step, catch_mt_cum_prop, catch_mt_cum_prop_obs) %>% 
      gather(key="type", value="cum_catch_prop", 2:3) %>% 
      mutate(type=recode(type, "catch_mt_cum_prop"="Simulated", "catch_mt_cum_prop_obs"="Observed"))
    
    # Plot total catch time series
    g3 <- ggplot(catch_stats1, aes(x=step, y=cum_catch_prop, group=type, color=type)) +
      geom_line() +
      labs(x="Week", y="Proprotion of catch", title="Proportion of catch over time") +
      theme_bw() + my_theme
    print(g3)
    
    # Effort
    #############################
    
    effort_stats <- data %>% 
      select(step, block_id, traps_n) %>% 
      unique() %>% 
      group_by(step) %>% 
      summarize(ntraps=sum(traps_n, na.rm=T))
    
    g4 <- ggplot(effort_stats, aes(x=step, y=ntraps/1000)) +
      geom_line() +
      labs(x="Week", y="Number of traps (1000s)", title="Effort over time") +
      theme_bw() + my_theme
    print(g4)
    
    
  }
  
  # 3. Terminal year population structure
  #############################################
  
  last_year_stats <- data %>% 
    filter(step==max(step)) %>% 
    group_by(age_yr, sex) %>% 
    summarize(biomass_n=sum(biomass_n))
  
  g4 <- ggplot(last_year_stats, aes(x=age_yr, y=biomass_n/1e6, fill=sex)) +
    geom_bar(stat="identity") +
    labs(x="Age (yr)", y="Number of crabs (millions)", title="Age/sex structure in the final time step") +
    scale_x_continuous(breaks=1:max(data$age_yr)) +
    theme_bw() + my_theme
  print(g4)


}
