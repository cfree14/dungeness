
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(lubridate)
library(tidyverse)

# Directories
codedir <- "models/popdy/v2/functions"
outputdir <- "output/raw"
plotdir <- "models/figures"

# Read helper functions
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Options:
# effort_dynamics: none, constant, biomass-coupled
# management: "none", "April 1 closure", "entanglement trigger", "marine life concentration trigger", "dynamic MLC block closures", "effort reduction"


# Run model
################################################################################

# No fishing
###################################################

# No fishing
data1 <- run_model(yrs2sim=2015, effort_dynamics="none")
plot_results(data1)



# Constant fishing
###################################################

# Constant fishing (no management)
data2 <- run_model(yrs2sim=2015, effort_dynamics="constant")
plot_closures(data2)
plot_results(data2)

# MLC closures
#############################

# Constant fishing (early closure)
data3 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="early closure")
plot_closures(data3)
plot_results(data3)

# Constant fishing (scheduled closures)
data4 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="scheduled closures")
plot_closures(data4)
plot_results(data4)

# Constant fishing (dynamic closures)
data5 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="dynamic closures")
plot_closures(data5)
plot_results(data5)

# Entanglement triggers
#############################

# Entanglement triggered closure
data6 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="entanglement trigger", nweeks=20)
plot_closures(data6)
plot_results(data6)

# Entanglement triggered gear reduction
data7 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="entanglement triggered gear reduction", mgmt_options=list(E_red_prop=0.5), nweeks=25)
plot_closures(data7)
plot_results(data7)



# Effort reduction
data4 <- run_model(yrs2sim=2015, effort_dynamics="constant", management="effort reduction", mgmt_options=list(E_red_prop=0.5))
plot_results(data3)

# Domoic management
data5 <- run_model(yrs2sim=2014:2018, effort_dynamics="constant", management="current domoic", 
                   mgmt_options=list(ncrabs_sampled=6, delay_thresh=3, reopen_thresh=0))
plot_closures(data5, zone="da", plot_name="2015_da_mgmt")
plot_results(data5)


# Realistic fishing
######################

# No management
###########################

# Run one year with biomass-coupled fishing (no management)
# Smaller b stretches slope, smaller a extends how long plateau lasts
data4 <- run_model(yrs2sim=2014, effort_dynamics="biomass-coupled", a=0.4, b=0.1)
plot_results(data4, plot_obs=F)
plot_closures(data4)    #, plot_name="2015_no_management_closures")

# Domoic managament
###########################

# Domoic management
data5 <- run_model(yrs2sim=2014:2018, effort_dynamics="biomass-coupled", a=0.4, b=0.1,
                   management=c("current domoic", "entanglement trigger"),
                   mgmt_options=list(ncrabs_sampled=6, delay_thresh=3, reopen_thresh=1))
plot_closures(data5, zone="ramp")
plot_results(data5)


# Whale management
###########################

# Entanglement trigger
data5 <- run_model(yrs2sim=2014:2018, effort_dynamics="biomass-coupled", a=0.4, b=0.1, management="entanglement trigger")
plot_results(data5)
plot_closures(data5)

# Scheduled closures
data5 <- run_model(yrs2sim=2014:2018, effort_dynamics="biomass-coupled", a=0.4, b=0.1, management="scheduled closures")
plot_closures(data5)
plot_results(data5)


# Entanglement triggered gear reduction
data5 <- run_model(yrs2sim=2014, effort_dynamics="biomass-coupled", a=0.4, b=0.1, 
                   management="entanglement triggered gear reduction", mgmt_options=list(E_red_prop=0.5), nweeks=25)
plot_results(data5)
plot_closures(data5)



# Run one year with biomass-coupled fishing (and management)
data5 <- run_model(yrs2sim=2015, effort_dynamics="biomass-coupled", a=0.4, b=0.1, management="marine life concentration trigger")
plot_results(data5, plot_obs=F)   #, "2015_mlc_trigger")
plot_closures(data5)   #, "2015_mlc_trigger_closures")

# Run one year with biomass-coupled fishing (and management)
data6 <- run_model(yrs2sim=2015, effort_dynamics="biomass-coupled", a=0.4, b=0.1, 
                   management=c("entanglement trigger", "marine life concentration trigger"))
plot_results(data6, plot_obs=F, plot_name="2015_mlc_and_entanglement_triggers")
plot_closures(data6, plot_name="2015_mlc_and_entanglement_trigger_closures")

# Run one year with biomass-coupled fishing (and management)
data7 <- run_model(yrs2sim=2015, effort_dynamics="biomass-coupled", a=0.4, b=0.1, 
                   management="dynamic MLC block closures")
plot_results(data7, plot_obs=F, plot_name="2015_dynamic_block_mgmt_0.03")
plot_closures(data7, plot_name="2015_dynamic_block_mgmt_closures_0.03")

# Run one year with biomass-coupled fishing (and management)
data8 <- run_model(yrs2sim=2015, effort_dynamics="biomass-coupled", a=0.4, b=0.1, 
                   management="effort reduction", mgmt_options = list(E_red_prop=0.7))
plot_results(data8, plot_obs=F, plot_name="2015_effort_reduction_70perc")
plot_closures(data8, plot_name="2015_effort_reduction_70perc_closures")


# Run model many times
################################################################################

# Setup parallel
library(doParallel)
ncores <- detectCores()
registerDoParallel(cores=ncores)

# Number of iterations
niter <- 50

# Scenarios
mgmt_scens <- list("none", 
                "April 1 closure", 
                "entanglement trigger", 
                "marine life concentration trigger", 
                c("entanglement trigger", "marine life concentration trigger"),
                "dynamic MLC block closures", 
                "effort reduction")

# Scenario file names
mgt_scen_names <-c("no_management", 
                   "Apr1_closure", 
                   "entanglement_trigger", 
                   "mlc_trigger", 
                   "mlc_entanglement_triggers",
                   "dynamic_mlc_block_closures",
                   "70%_effort_reduction")

# Loop through scenarios
j <- 1
for(j in 1:length(mgmt_scens)){

  # 
  mgmt_vec <- mgmt_scens[[j]]
  mgmt_name <- mgt_scen_names[j]
  
  # Loop through iterations
  i <- 1
  foreach(i=1:niter) %dopar% {
    
    # Run model
    # "none", "April 1 closure", "entanglement trigger", "marine life concentration trigger", "dynamic MLC block closures"
    output <- run_model(yrs2sim=2014:2018, effort_dynamics="biomass-coupled", a=0.4, b=0.1, management=mgmt_vec, mgmt_options=list(E_red_prop=0.7))
    
    # Export results
    outfile <- paste0("Scenario", j, "_", mgmt_name, "_", stringr::str_pad(i, width=2, side="left", pad="0"), ".Rds")
    saveRDS(output, file.path(outputdir, outfile))
    
  }

}




# Merge results
################################################################################

# Merge results and plot
results <- merge_results(scenario="Scenario2_Apr1closure")
stats <- plot_merged_results(results)




# Merge and compare results
################################################################################

# Merge results
results <- merge_scenario_results(scenarios=c("Scenario1_no_management",
                                              "Scenario2_Apr1closure"))

# Plot merged results


