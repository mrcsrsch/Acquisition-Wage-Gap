##################################################################################
# Clean polis
##################################################################################
#### output dirs #####
if (!dir.exists(paste0(map_data, "step4/"))) dir.create(paste0(map_data, "step4/"))
map_output_here <- paste0(map_data, "step4/")
##################################################################################
##################################################################################

#### read in yearly POLIS sequentially ----
RINPs_year <- list()
t <- 0
# READ IN RINPs_year 
for (current_year in 2006:2018){
  # message
  cat(paste(Sys.time(), "Starting with year:", current_year), "\n")
  
  t <- t+1
  path <- paste0(map_data,"step3/SPOLIS_yearly/SPOLIS_yearly_", current_year, ".rds")
  RINPs_year[[t]] <- readRDS(path)
  
}
RINPs_year <- rbindlist(RINPs_year)
rm(t, path)
gc()

# remove observations with missing SBEID (all in 2008 and already in csv)
RINPs_year <- RINPs_year[SBEID!="", ]

#### add tcBEID identifier ----
trans_SBEID_tcBEID <- readRDS(paste0(map_data, "step2/trans_SBEID_tcBEID.rds"))
RINPs_year <- merge(RINPs_year, trans_SBEID_tcBEID, by=c("SBEID", "year"), all.x=TRUE)
rm(trans_SBEID_tcBEID)
# fix in case any tcBEIDs are missing
RINPs_year[is.na(tcBEID), tcBEID := SBEID]

#### Create indicators for subsetting ----
# add log wages
RINPs_year[, lrhwage := log(rhwage)]

# per person: number of observations, min and max hourly wage, log rhwage difference
setkeyv(RINPs_year, c("RINP", "year"))
RINPs_year[, c("obs", "min.rhwage", "max.rhwage", "lrhwage.diff") := list(.N, min(rhwage), 
                                                                          max(rhwage), 
                                                                          lrhwage-shift(lrhwage, n=1, fill=NA, type=c("lag"))), by=RINP]
RINPs_year[is.na(lrhwage.diff), lrhwage.diff:=0 ]

# per person-firm: number of observations
RINPs_year[, firm.obs := .N, by=c("RINP", "tcBEID")]

#### Subset data set ----
### delete full earnings history of workers if ###
dropped <- matrix(NA, nrow=4, ncol=4)
colnames(dropped) <- c("workers", "firms.SBEID", "firms.tcBEID", "obs")
rownames(dropped) <- c("high/low wage", "log(wage) change", "one obs", "outside age range")

# hourly wage in any year is extremely high or extremely low
dropped[1,] <- as.vector(unlist(RINPs_year[min.rhwage < 5 | max.rhwage > 1000, .(worker=uniqueN(RINP), firms_SBEID=uniqueN(SBEID), firm_tcBEID = uniqueN(tcBEID), obs=.N)]))

# change in year-on-year log hourly wage is >1 or <-1
RINPs_year[, drop.diff := fifelse((any(lrhwage.diff>1) | any(lrhwage.diff<(-1))), TRUE, FALSE), by=RINP]
dropped[2,] <- as.vector(unlist(RINPs_year[drop.diff==TRUE, .(worker=uniqueN(RINP), firms_SBEID=uniqueN(SBEID), firm_tcBEID = uniqueN(tcBEID), obs=.N)]))

# there is only one person-year observation
dropped[3,] <- as.vector(unlist(RINPs_year[obs==1, .(worker=uniqueN(RINP), firms_SBEID=uniqueN(SBEID), firm_tcBEID = uniqueN(tcBEID), obs=.N)]))

### delete earnings of workers ###

# with age <20 or age > 60
dropped[4,] <- as.vector(unlist(RINPs_year[age < 20 | age > 60, .(worker=uniqueN(RINP), firms_SBEID=uniqueN(SBEID), firm_tcBEID = uniqueN(tcBEID), obs=.N)]))

### now actually delete them ###
RINPs_year <- RINPs_year[min.rhwage >= 5 & max.rhwage <= 1000, !c("min.rhwage", "max.rhwage")]
RINPs_year <- RINPs_year[drop.diff==FALSE, !c("drop.diff")]
RINPs_year <- RINPs_year[obs > 1, !c("obs")]
RINPs_year <- RINPs_year[age>=20 & age<=60, ]

# factorize 
RINPs_year[, year:=factor(year)]
RINPs_year[, RINP := factor(RINP)]
RINPs_year[, SBEID := factor(SBEID)]
RINPs_year[, tcBEID := factor(tcBEID)]
gc()

# Save full data set
saveRDS(RINPs_year, paste0(map_output_here,"RINP_tcBEID.rds"), compress=TRUE)
rm(RINPs_year)

# save subset information
saveRDS(dropped, paste0(map_data,"step4/info_dropped.rds"), compress=TRUE)
rm(dropped)





