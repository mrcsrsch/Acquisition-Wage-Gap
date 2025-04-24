##################################################################################
# Create yearly SPOLIS for main analysis
##################################################################################
#### packages ######
if (!require("parallel")) install.packages("parallel"); library("parallel")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

#### output dirs #####
if (!dir.exists(paste0(map_data, "step3/"))) dir.create(paste0(map_data, "step3/"))
if (!dir.exists(paste0(map_data, "step3/SPOLIS_yearly/"))) dir.create(paste0(map_data, "step3/SPOLIS_yearly/"))
map_output_here <- paste0(map_data, "step3/SPOLIS_yearly/")
##################################################################################
##################################################################################
# Read in Parallel processing
numCores <- 2 #detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

## run parallel
result <- foreach (current_year=2006:2018, .packages=c("data.table", "lubridate")) %dopar% {
  path <- paste0(map_data, "step1_basics/subset_SPOLIS/SPOLIS_employees_", current_year, ".rds")
  SPOLIS <- readRDS(path)
  rm(path)
  
  # aggregate jobs at same employer within one month
  ## adjust day in AANVSSBBUS
  SPOLIS[, AANVSSBBUS := ymd(paste0(current_year, "-", month(AANVSSBBUS), "-01"))]

  ## change contractsoort to numeric 
  SPOLIS[SCONTRACTSOORT == "O" | SCONTRACTSOORT == "o", SCONTRACTSOORT := "1"]
  SPOLIS[SCONTRACTSOORT == "B" | SCONTRACTSOORT == "b", SCONTRACTSOORT := "2"]
  SPOLIS[SCONTRACTSOORT == "N" | SCONTRACTSOORT == "n", SCONTRACTSOORT := "9"]
  SPOLIS[, SCONTRACTSOORT := as.integer(SCONTRACTSOORT)]
  gc()
  
  ## aggregate over month, person, employer combination (multiple jobs at same employer possible)
  SPOLIS[, `:=`(SBASISLOON = sum(SBASISLOON),
                SBIJZONDEREBELONING = sum(SBIJZONDEREBELONING),
                SLNOWRK = sum(SLNOWRK),
                SOVERWERKUREN = sum(SOVERWERKUREN),
                SREGULIEREUREN = sum(SREGULIEREUREN),
                SCONTRACTSOORT = min(SCONTRACTSOORT)), 
         by = c("AANVSSBBUS", "SBEID", "RINP")]
  
  ### delete duplicates 
  SPOLIS <- SPOLIS[!duplicated(SPOLIS, by = NULL)]

  # delete jobs with non-positive hours/wage: these e.g. occur because of remaining holidays or paybacks 
  SPOLIS <- SPOLIS[SREGULIEREUREN > 0 & SBASISLOON > 0 & SBIJZONDEREBELONING >= 0 & SLNOWRK >= 0 & SOVERWERKUREN >= 0 ,]
  gc()
  
  ## calculate nominal hourly wage 
  SPOLIS[, hwage := (SBASISLOON+SBIJZONDEREBELONING+SLNOWRK)/(SOVERWERKUREN+SREGULIEREUREN)]
  SPOLIS[, hwage_basic := SBASISLOON/SREGULIEREUREN]
  gc()
  
  # aggregate over year, person, employer combination to get yearly panel
  SPOLIS <- SPOLIS[, .(hwage = mean(hwage),
                       hwage_basic = mean(hwage_basic),
                       SBASISLOON = sum(SBASISLOON),
                       SBIJZONDEREBELONING = sum(SBIJZONDEREBELONING),
                       SLNOWRK = sum(SLNOWRK),
                       SOVERWERKUREN = sum(SOVERWERKUREN),
                       SREGULIEREUREN = sum(SREGULIEREUREN),
                       SCONTRACTSOORT = min(SCONTRACTSOORT),
                       months_spell = .N), by=c("SBEID", "RINP")]
  SPOLIS[, year := as.integer(current_year)]
  gc()
  
  # calculate real hourly wage per year
  
  ## load inflation factor: currently set to 2018 according to cpi
  inflation_factor <- readRDS(paste0(map_data, "step1_basics/cpi_year.rds"))
  inflation_factor <- inflation_factor[year==current_year, factor]
  ## look up factor and calculate real wage
  SPOLIS[, factor := inflation_factor]
  SPOLIS[, rhwage := hwage*factor]
  SPOLIS[, rhwage_basic := hwage_basic*factor]
  SPOLIS[, c("factor") := NULL]
  
  ## delete again 
  rm(inflation_factor)
  gc()
  
  # determine main job per year
  # how many workers with >1 job / year?
  
  if (!dir.exists(paste0(map_data, "step3/"))) dir.create(paste0(map_data, "step3/"))
  tt <- SPOLIS[, .(employers=uniqueN(SBEID), months=sum(months_spell)), by=RINP] 
  share_double <- tt[, sum(employers>1)/.N] # total share of doubles
  share_double_12 <- tt[, sum(months>12)/.N] # share of workers holding 2 jobs at once
  write(paste0(Sys.time(), ";", current_year, ";", share_double, ";", share_double_12, "\n"), file=paste0(map_data,"step3/share_multiple_jobs.txt"), append=TRUE)
  rm(tt, share_double, share_double_12)
  gc()
  ## most regular salary max(SBASISLOON)
  # save obs in vector
  obs_vec <- nrow(SPOLIS)
  
  ## Card et al. (2013) for yearly; Dauth et. al (2019) (see Oberschlachtsiek, Scioch, Seysen and Heining (2009))  
  
  SPOLIS <- SPOLIS[SPOLIS[, .I[SBASISLOON==max(SBASISLOON)], 
                          by=c("RINP")]$V1]
  obs_vec[2] <- nrow(SPOLIS)
  
  ## most regular hours (SREGULIEREUREN)
  ### SPOLIS <- SPOLIS[, .SD[which(SREGULIEREUREN==max(SREGULIEREUREN))], 
  ###         by=c("month", "RINPERSOONS","RINPERSOON")]
  SPOLIS <- SPOLIS[SPOLIS[, .I[SREGULIEREUREN==max(SREGULIEREUREN)], 
                          by=c("RINP")]$V1]
  obs_vec[3] <- nrow(SPOLIS)
  
  ## best contract min(SCONTRACTSOORT)
  SPOLIS <- SPOLIS[SPOLIS[, .I[SCONTRACTSOORT==min(SCONTRACTSOORT)], 
                          by=c("RINP")]$V1]
  obs_vec[4] <- nrow(SPOLIS)
  
  ## held for longest time
  SPOLIS <- SPOLIS[SPOLIS[, .I[months_spell==max(months_spell)], 
                          by=c("RINP")]$V1]
  obs_vec[5] <- nrow(SPOLIS)
  
  ## rest: oldest SBEID = oldest company
  SPOLIS <- SPOLIS[SPOLIS[, .I[SBEID==min(SBEID)], 
                          by=c("RINP")]$V1]
  obs_vec[6] <- nrow(SPOLIS)
  
  # write obs reduction to file
  write(paste0(Sys.time(), ";", "Main job obs reduction;", current_year, ";", paste0(obs_vec, collapse=";")),
               file=paste0(map_data,"step3/picking_main_job.txt"), append=TRUE)
  rm(obs_vec)
  
  # delete what is not necessary
  SPOLIS[, c("SBASISLOON", "SBIJZONDEREBELONING",
             "SCONTRACTSOORT", "SLNOWRK",
             "SOVERWERKUREN", "SREGULIEREUREN", "months_spell") := NULL]
  gc()
  
  # Merge with other data
  ## merge with GBA
  gba <- readRDS(paste0(map_data, "step1_basics/gba.rds"))
  gba[, gba:=NULL]
  gba[, female := as.logical(female)]
  gba[, gebyear := as.integer(year(gebdat))]
  gba[, gebdat := NULL]
  
  SPOLIS <- merge(SPOLIS, gba, by=c("RINP"), all.x = TRUE)
  rm(gba)
  gc()
  
  ## calculate age
  SPOLIS[, age := year-gebyear]
  SPOLIS[, gebyear:=NULL]
  gc()
  
  # Delete very old and extremely young individuals
  SPOLIS <- SPOLIS[age>=15 & age<=75, ]
  
  # SAVE individually     
  saveRDS(SPOLIS, file=paste0(map_output_here,"SPOLIS_yearly_", current_year, ".rds"), compress=FALSE)

  # clean up
  rm(SPOLIS)
  gc()
}


# stop clusters
stopCluster(cl)
rm(cl)


