##################################################################################
# create monthly SPOLIS for tcBEID analysis in script 2B
##################################################################################
#### packages ######
if (!require("parallel")) install.packages("parallel"); library("parallel")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

#### output dirs #####
if (!dir.exists(paste0(map_data, "step2_yearly/"))) dir.create(paste0(map_data, "step2_yearly/"))   
if (!dir.exists(paste0(map_data, "step2_yearly/fin_SPOLIS/"))) dir.create(paste0(map_data, "step2_yearly/fin_SPOLIS/"))
map_output_here <- paste0(map_data, "step2_yearly/fin_SPOLIS/")

##################################################################################
##################################################################################
# Read in Parallel processing
## the loop below is contructed to run in parallel. 
numCores <- 2 #detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

# combine function
comb <- function(x, ...) {  
      mapply(rbind,x,...,SIMPLIFY=FALSE)
}

## run parallel
result <- foreach (current_year=2006:2018, .packages=c("data.table", "lubridate"), .combine='comb', .multicombine=TRUE) %dopar% {
    path <- paste0(map_data, "step1_basics/subset_SPOLIS/SPOLIS_employees_", current_year, ".rds")
    SPOLIS <- readRDS(path)
    rm(path)

    # aggregate jobs at same employer
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
    gc()
    
    # delete jobs with non-positive hours/wage: these e.g. occur because of remaining holidays or paybacks 
    SPOLIS <- SPOLIS[SREGULIEREUREN > 0 & SBASISLOON > 0 & SBIJZONDEREBELONING >= 0 & SLNOWRK >= 0 & SOVERWERKUREN >= 0 ,]
    gc()
    
    # determine main job per month (Note: This is necessary to calculate flows for tcBEIDs in the next script. Later I find a main employer per year.)
    ## first record the average number of jobs per person in a month
    avg_SBEID <- SPOLIS[, .(.N, 1),
                        by=c("AANVSSBBUS", "RINP")][,
                                                    .(avgSBEIDs=mean(N), RINPs=sum(V2)), by="AANVSSBBUS"]
    
    
    ## most regular salary max(SBASISLOON)
    ## Card et al. (2013) for yearly; Dauth et. al (2019) (see Oberschlachtsiek, Scioch, Seysen and Heining (2009))  
    SPOLIS <- SPOLIS[SPOLIS[, .I[SBASISLOON==max(SBASISLOON)], 
                            by=c("AANVSSBBUS", "RINP")]$V1]

    ## most regular hours (SREGULIEREUREN)
    SPOLIS <- SPOLIS[SPOLIS[, .I[SREGULIEREUREN==max(SREGULIEREUREN)], 
                            by=c("AANVSSBBUS", "RINP")]$V1]

    ## best contract min(SCONTRACTSOORT)
    SPOLIS <- SPOLIS[SPOLIS[, .I[SCONTRACTSOORT==min(SCONTRACTSOORT)], 
                            by=c("AANVSSBBUS", "RINP")]$V1]

    ## rest: oldest SBEID = oldest company
    SPOLIS <- SPOLIS[SPOLIS[, .I[SBEID==min(SBEID)], 
                            by=c("AANVSSBBUS", "RINP")]$V1]

    # delete what is not necessary
    SPOLIS[, c("SBASISLOON", "SBIJZONDEREBELONING",
               "SCONTRACTSOORT", "SLNOWRK",
               "SOVERWERKUREN", "SREGULIEREUREN") := NULL]

    # SAVE individually  
    saveRDS(SPOLIS, file=paste0(map_output_here, "fin_SPOLIS_", current_year,".rds"))                  
    
    # clean up
    rm(SPOLIS)
    gc()
    
    # return observations
    return(list(avg_SBEID=avg_SBEID))
  }

# stop clusters
stopCluster(cl)
rm(cl)

# create observations table 
write.csv2(result$avg_SBEID, file = paste0(map_output_here, "final_SPOLIS_avgSBEID.csv"))