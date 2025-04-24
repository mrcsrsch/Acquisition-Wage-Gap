##################################################################################
# Fit the AKM model and retrieve the fixed effects
##################################################################################
### Packages ####
if (!require("lfe")) install.packages("lfe"); library("lfe")
# lfe options
options(lfe.pint = 300) # life sign every 5 minutes
options(lfe.threads = 6) # just in case you want to center more vectors at once
options(lfe.accel = 0) #1
options(lfe.eps = 10^(-8)) # set this to 10^-8 or so later


### output dir ####
if (!dir.exists(paste0(map_data, "step6/"))) dir.create(paste0(map_data, "step6/"))
if (!dir.exists(paste0(map_data, "step6/yeartcBEID/"))) dir.create(paste0(map_data, "step6/yeartcBEID/"))
map_output_here <- paste0(map_data, "step6/yeartcBEID/")


### read data ####
RINPs_year <- readRDS(paste0(map_data,"step6/yeartcBEID/RINPs_year_before_AKM.rds"))
##################################################################################
##################################################################################

# fit model
result_resid <- felm(resid_1 ~ 0 | RINP + tcBEIDyear, data=RINPs_year)
gc()
saveRDS(result_resid, paste0(map_output_here,"result_resid.rds"), compress=FALSE)

# add prediction and residuals to RINPs_year
RINPs_year[, pred.AKM := result_resid$fitted.values]
RINPs_year[, resid.AKM := result_resid$residuals]

#### Retrieve fixed effects and add them to the data.table ----
# retrieve FEs
AKM_FEs <- getfe(result_resid, ef='ref')
saveRDS(AKM_FEs, paste0(map_output_here,"AKM_FEs.rds"), compress=FALSE)
AKM_FEs <- setDT(AKM_FEs)
rm(result_resid)
gc()

# add to RINPs_year
RINPs_year <- merge(RINPs_year, AKM_FEs[fe=="tcBEIDyear", .(obs_firm_FE=obs, tcBEIDyear=idx, firm_FE=effect)], by=c("tcBEIDyear"))
RINPs_year <- merge(RINPs_year, AKM_FEs[fe=="RINP", .(obs_worker_FE=obs, RINP=idx, worker_FE=effect)], by=c("RINP"))
RINPs_year[, tcBEIDyear := NULL]
gc()

#### save result ----
saveRDS(RINPs_year, paste0(map_output_here,"tcBEIDyear_AKM.rds"), compress=FALSE)
rm(RINPs_year)


