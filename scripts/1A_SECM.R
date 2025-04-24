##################################################################################
# Read in and prepare SECM
##################################################################################
#### packages ######
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("parallel")) install.packages("parallel"); library("parallel")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

#### output dirs #####
if (!dir.exists(paste0(map_data, "step1_basics/"))) dir.create(paste0(map_data, "step1_basics/"))
if (!dir.exists(paste0(map_data, "step1_basics/SECM/"))) dir.create(paste0(map_data, "step1_basics/SECM/"))
map_output_here <- paste0(map_data, "step1_basics/SECM/")
##################################################################################
##################################################################################

# Read in data ====
path <- paste0(map_data2,"SECM_MAANDELIJKS/2018/DATA/SECMBUSANAV1.CSV")
SECM <- fread(path, sep=";", colClasses = c("character",
                                            "character",
                                            "character", 
                                            "character",
                                            "integer",
                                            "NULL"))
rm(path)

# put RINPERSOONS and RINPERSOON together =====
setnames(SECM, 1, "RINPERSOONS")
SECM[, RINP := paste0(RINPERSOONS, RINPERSOON)]
SECM[, c("RINPERSOONS", "RINPERSOON") := NULL]
gc()

# only keep those that are employees at least once
# I need to do this again below but it is handy here because of the lengthy collapse call
SECM <- SECM[, if(any(SECM==11)) .SD, by="RINP"]

# Fix date formats ====
SECM[, AANVSECM := ymd(AANVSECM)]
SECM[, EINDSECM := ymd(EINDSECM)]

# For some reason sometimes spells of the same SECM are seperated, fix this here
# this is essential because further down I exploit this structure
## sort
setorderv(SECM, c("RINP", "AANVSECM"), order=c(1,1))

# collapse adjoining spells: should take about 45 min! ====
SECM[, EINDSECM := collapse(SECM, AANVSECM, EINDSECM), by=RINP]
SECM <- SECM[EINDSECM!=ymd("3030-03-03"),]

# clean up memory
gc()

##################################################################################
# turn into monthly observations per year ----

## Start parallel processing ----
### the loop below is contructed to run in parallel. 
numCores <- 2 #detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

## export whole SECM to cluster (multiplies memory usage!) ====
clusterExport(cl, c("SECM"), envir=environment())

## loop in parallel ----

obs <- foreach (current_year=2006:2018, .packages=c("data.table", "lubridate"), .combine=rbind) %dopar% {
  
  # take out current year and only obs of employees
  current <- SECM[year(AANVSECM)<=current_year & year(EINDSECM)>=current_year & SECM==11, ]
  
  
  # impute beginning and end of year for longer records
  current[year(AANVSECM)<current_year, AANVSECM := ymd(paste0(current_year,"-01-01"))]
  current[year(EINDSECM)>current_year, EINDSECM := ymd(paste0(current_year,"-12-31"))]
  gc()
  
  # create monthly observations
  current <- current[, list(RINP = RINP, 
                            SECM = SECM, 
                            SECM_date = seq(AANVSECM, EINDSECM, by="month")), 
                     by=1:nrow(current)]
  current[,nrow:=NULL]
  obs <- current[,.N]
  obs[2] <- current[, uniqueN(RINP)]
  
  # SAVE employees 
  current[, SECM:=NULL]
  obs[3] <- current[,.N]
  obs[4] <- current[, uniqueN(RINP)] 
  

  ## SAVE
  saveRDS(current, file=paste0(map_output_here, "SECM_employees_monthly_", current_year,".rds"))
  
  rm(current)
  gc()
  
  return(c(current_year, obs))
}

# stop clusters
stopCluster(cl)
rm(cl)

# clean up 
rm(SECM, obs)
gc()