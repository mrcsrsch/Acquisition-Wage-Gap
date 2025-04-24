##################################################################################
# SPOLIS, yearly 2006-2018: Read in and subset to SECM status 11 (full time employee)
##################################################################################
#### packages ######
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("parallel")) install.packages("parallel"); library("parallel")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

#### output dirs #####
if (!dir.exists(paste0(map_data, "step1_basics/subset_SPOLIS/"))){dir.create(paste0(map_data, "step1_basics/subset_SPOLIS/"))}
map_output_here <- paste0(map_data, "step1_basics/subset_SPOLIS/")

##################################################################################
##################################################################################
# Read in parallel processing session
## the loop below is contructed to run in parallel.
numCores <- 2 #detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

## load some information into the clusters
col_names <- c("RINPERSOONS", "RINPERSOON", "IKVID", 
               "AANVSSBBUS", "EINDSSBBUS", "SARBEIDSRELATIE", 
               "SBAANDAGEN", "SBASISLOON", "SBEID", 
               "SBIJZONDEREBELONING", "SCONTRACTSOORT", "SDATUMAANVANGIKO", 
               "SDATUMEINDEIKO", "SLNOWRK", "SOVERWERKUREN", "SREGULIEREUREN", 
               "SSOORTBAAN", "SVOLTIJDDAGEN", "jaar")
keep <- c(1,1,0,
          1,0,0,
          0,1,1,
          1,1,0,
          0,1,1,1,
          0,0,0)
numerics <- c(0, 0, 0,
              0, 0, 0,
              0, 1, 0,
              1, 0, 0,
              0, 1, 1, 1,
              0, 0, 0)
integers <- c(0, 0, 0,
              0, 0, 0,
              0, 0, 0,
              0, 0, 0,
              0, 0, 0, 0,
              0, 0, 1)
colClass <- ifelse(keep==1, ifelse(numerics==1, "numeric", 
                                   ifelse(integers==1, "integer", "character")), "NULL")
col_names <- col_names[keep==1]
rm(keep, numerics, integers)


# Export to clusters
clusterExport(cl, c("col_names", "colClass"), envir=environment())

## run parallel
obs <- foreach (current_year=2006:2018, .packages=c("data.table", "lubridate"), .combine=rbind) %dopar% {
  path <- ifelse(current_year < 2010, paste0(map_data2,"POLIS/", current_year, "/DATA/", "POLISBUS", current_year, "BUSV1.csv"), 
                 paste0(map_data2,"POLIS/", current_year, "/DATA/", "SPOLISBUS", current_year, "BUSV1.csv"))
  SPOLIS <- fread(path, dec=",", sep=";",
                  colClasses = colClass)
  rm(path)
  
  # fix some name problems
  if (current_year<2010) setnames(SPOLIS, 1:length(col_names), col_names) else setnames(SPOLIS, 1, "RINPERSOONS")
  
  # put RINPERSOONS and RINPERSOON together - this saves about 700 MB Ram / year
  SPOLIS[, RINP := paste0(RINPERSOONS, RINPERSOON)]
  SPOLIS[, c("RINPERSOONS", "RINPERSOON") := NULL]
  gc()
  
  # some formatting
  SPOLIS[, AANVSSBBUS := ymd(AANVSSBBUS)] 
  SPOLIS[, month := as.integer(month(AANVSSBBUS))]
  SPOLIS[, SPOLIS_ID := 1]
  gc()
  
  # save obs
  obs <- SPOLIS[, .N]
  obs[2] <- SPOLIS[, uniqueN(RINP)]
  
  ## load SECM employees
  SECM_employ <- readRDS(paste0(map_data, 
                                "step1_basics/SECM/SECM_employees_monthly_", 
                                current_year, ".rds"))
  SECM_employ[, month := as.integer(month(SECM_date))]
  SECM_employ[, SECM_date := NULL]
  gc()
  
  ## merge with employees and SECM. Only keep those. (= workers whose main source if income is employment)
  SPOLIS <- merge(SECM_employ, SPOLIS, by=c("RINP", "month"), all.x = TRUE)
  rm(SECM_employ)
  gc()
  
  # remove non-matched (those are employed abroad)
  SPOLIS <- SPOLIS[!is.na(SPOLIS_ID), ]
  obs[3] <- SPOLIS[, .N]
  obs[4] <- SPOLIS[, uniqueN(RINP)]
  SPOLIS[, month := NULL]
  SPOLIS[, SPOLIS_ID := NULL]
  gc()

  # SAVE individually                
  saveRDS(SPOLIS, file=paste0(map_output_here, "SPOLIS_employees_", current_year,".rds")) 
  
  # clean up
  rm(SPOLIS)
  gc()
  
  # return observation numbers and missings
  return(c(current_year, obs))
  
}
# stop clusters
stopCluster(cl)
rm(cl)

# save observations table 
colnames(obs) <- c("year", "full SPOLIS", "full SPOLIS RINP",
                   "FINAL SPOLIS", "FINAL SPOLIS RINP")
write.csv2(obs, file = paste0(map_output_here, "SPOLIS_employees_observations.csv"))