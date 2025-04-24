##################################################################################
##### 'Do workers or firms drive the foreign acquisition wage gap?' - master script #
# This script loads global settings and
# sources all other scripts in the folder to 
# i) build the analysis dataset
# and ii) run the analysis
# Setting "run.scripts" to TRUE will execute the full project.
##################################################################################
##################################################################################

# clean start 
rm(list=ls(all.names = TRUE))
gc() # return memory to system

# track memory usage
# gcinfo(FALSE)
# increase the memory limit to server maximum
memory.limit(size=55500)

# global packages
if (!require("data.table")) install.packages("data.table"); library("data.table")

# create folder structure and set paths
## SET YOUR MAIN FOLDER AND DATA SOURCE FOLDER HERE
main_path <- NA 
if (is.na(main_path)) warning("You need to set the main folder path.")

## set paths
map_scripts <- paste0(main_path, "scripts/")
map_data <- paste0(main_path, "data/")
map_data2 <- paste0(map_data, "data/source_data/")
map_output <-  paste0(main_path, "outputs/")

## create data and output paths if necessary
if (!dir.exists(map_data_analysis)) dir.create(map_data_analysis)
if (!dir.exists(map_output)) dir.create(map_output)


##################################################################################
### Call scripts to run analysis ####

run.scripts <- FALSE # set to TRUE to execute full project.
countert <- 0
ttimers <- list()

# small functions to wrap around script calls
start.part <- function(){
  # save and show timer
  countert <<- countert + 1 # update in global environment
  ttimers[[countert]] <<- Sys.time()
  cat(paste(ttimers[[countert]]), "\n")
}


end.part <- function(){
  # save and show timer
  ttimers[[countert]] <<- Sys.time()-ttimers[[countert]]
  print(ttimers[[countert]])
  # store list of objects to remove
  objs <<- ls(all.names = TRUE)[-which(ls(all.names = TRUE) %in% c("map_data_analysis", "map_data_source", "map_scripts", "map_output", "countert", "ttimers", "end.part", "start.part", "run.scripts"))]
}

if (run.scripts){
  ###########################################################################################################

  
  ### 1. Load and clean raw data ####

  #### GBA (worker demographics) ####
  start.part()
  source(paste0(map_scripts, "01_GBA.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Consumer price index ####
  start.part()
  source(paste0(map_scripts, "1_cpi_year.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### Socio-Economic Status ####
  start.part()
  source(paste0(map_scripts, "1A_SECM.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  #### POLIS (matched employer-employee data) ####
  start.part()
  source(paste0(map_scripts, "1B_SPOLIS_SECM.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 2. Identify time-consistent firm IDs from POLIS ####
  start.part()
  source(paste0(map_scripts, "2A_find_tcBEIDs_1.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
   
  start.part()
  source(paste0(map_scripts, "2B_find_tcBEIDs_2.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 3. Aggregate POLIS to worker-firm-years ####
  start.part()
  source(paste0(map_scripts, "3_SPOLIS_yearly.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 4. Clean POLIS ####
  start.part()
  source(paste0(map_scripts, "4_clean_POLIS.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 5. Find largest connected set ####
  start.part()
  source(paste0(map_scripts, "5_networks.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 6. Fit AKM model ####
  start.part()
  source(paste0(map_scripts, "6A_AKM_tcBEIDyear.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "6B_AKM_tcBEIDyear.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 7. Load, prepare and add firm-level data ####
  start.part()
  source(paste0(map_scripts, "7_firmlevel_data.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 8. Perform Propensity Score Matching and create analysis dataset ####
  start.part()
  source(paste0(map_scripts, "8A_PSM.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "8B_analysis_dataset.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  ### 9. Assign numeric IDs across all datasets ####
  start.part()
  source(paste0(map_scripts, "9_assign_numeric_IDs.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  ### 10. Run main analysis ####
  start.part()
  source(paste0(map_scripts, "10_main_result_size_industry.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "10_summary_statistics.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "10_xsec_decompositions.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "10_no_matching_DiD.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "10_firmFE_mechanisms.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  start.part()
  source(paste0(map_scripts, "10_hires_separations.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()

  # save timers
  saveRDS(ttimers, file=paste0(map_data_analysis, "project_timers.rds"))
}
