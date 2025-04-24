##################################################################################
# Prepare the matched data sets for the analysis #
##################################################################################
### output dir ####
if (!dir.exists(paste0(map_data, "step8/"))) dir.create(paste0(map_data, "step8/"))
map_output_here <- paste0(map_data, "step8/")


##################################################################################
##################################################################################

#### Read in data ----
tcBEIDs_year_ana <- readRDS(paste0(map_data,"step7/tcBEIDs_year_matching.rds"))
tcBEIDs_year_ana <- tcBEIDs_year_ana[, c("tcBEID", "year", "time_takeover",    "active_since",    "active_until",
                                                                           "log_wage", "mean_lrhwage", "firm_FE",    "workerFE",       "resid_1",  
                                                                           "workerFE_in", "workerFE_out",       "workerFE_var",       "workers",
                                                                           "nace", "nace_2digit", "nace21",                        
                                                                           "OG_SBEIDs",   "pre_knowledge", "log_employment",                                         
                                                                           "NA_before")]
gc()

# Load tcBEIDs_year from the previous script
overview <- list()
overview[[1]] <- list(tcBEIDs_year=readRDS(paste0(map_data,"step8/tcBEIDs_ana_PSM_z_1.rds")))

fit <- readRDS(paste0(map_data,"step6/yeartcBEID/fit_age_year.rds"))
fit2 <- fit$coefficients
rm(fit)
fit <- list(coefficients=fit2)
rm(fit2)

RINPs_year <- readRDS(paste0(map_data,"step7/RINPs_year.rds"))
gc()

# define movers
setorderv(RINPs_year, c("RINP", "year"))
RINPs_year[, c("year.lag", "year.lead", "tcBEID.lag", "tcBEID.lead") := shift(.(year, tcBEID), n=c(1, -1), fill=NA, type="lag"), by=RINP]
RINPs_year[, inmove_mover := (!is.na(tcBEID.lag) & tcBEID!=tcBEID.lag & year.lag==(year-1))]
RINPs_year[, outmove_mover := (!is.na(tcBEID.lead) & tcBEID!=tcBEID.lead & year.lead==(year+1))]
RINPs_year[, inmove_NA :=  (is.na(tcBEID.lag) | year.lag!=(year-1))]
RINPs_year[, outmove_NA := (is.na(tcBEID.lead) | year.lead!=(year+1))]

# loop through overview list and create tcBEIDs_ana tables
for (h in 1:length(overview)){ #

  cat(paste(Sys.time(), "Starting with", h, "\n"))
# Create analysis data sets
tcBEIDs_ana <- merge(tcBEIDs_year_ana, overview[[h]]$tcBEIDs_year, by=c("tcBEID"), all.x=TRUE)
tcBEIDs_ana[control_group==TRUE, time_takeover:=year-placebo_takeover_year]
tcBEIDs_ana <- tcBEIDs_ana[treatment_group2==TRUE | control_group==TRUE, c("tcBEID", "year",  "treatment_group2",  "time_takeover",    "active_since",    "active_until",
                                                                           "log_wage", "mean_lrhwage", "firm_FE",    "workerFE",       "resid_1",  
                                                                           "workerFE_in", "workerFE_out",       "workerFE_var",       "workers",
                                                                           "nace", "nace_2digit", "nace21", "log_employment",                       
                                                                           "OG_SBEIDs",   "pre_knowledge",                                         
                                                                           "stratum", "matching_group",              
                                                                           "NA_before.x",  "NA_before.y")]
setkeyv(tcBEIDs_ana, c("tcBEID", "year"))

# create RINPs_ana
RINPs_ana <- merge(RINPs_year, overview[[h]]$tcBEIDs_year, by=c("tcBEID"), all.x=TRUE)
RINPs_ana[control_group==TRUE, time_takeover:=year-placebo_takeover_year]
RINPs_ana <- RINPs_ana[treatment_group2==TRUE | control_group==TRUE,]
setkeyv(RINPs_ana, c("RINP", "year"))


#### Vars to still extract from RINPs_ana ----

#### Calculate wage components for all, stayers, inmovers & outmovers ----
# add intercept 
RINPs_ana[, intercept := fit$coefficients[grepl("Intercept", names(fit$coefficients))]]

# add year FE
RINPs_ana[, year_FE :=  fit$coefficients[grepl(paste0(year), names(fit$coefficients))], by=year]
RINPs_ana[year==2006, year_FE:=0]


# add age profile
RINPs_ana[, age.profile := 
            (age-40)*fit$coefficients[grepl("age_1", names(fit$coefficients))]  +
            (age-40)^2*fit$coefficients[grepl("age_2", names(fit$coefficients))]  +
            (age-40)^3*fit$coefficients[grepl("age_3", names(fit$coefficients))], by=seq_len(nrow(RINPs_ana))]

# predictions
RINPs_ana[, lrhwage.pred := intercept + year_FE+ worker_FE+ firm_FE+ age.profile]

# residual
RINPs_ana[, resid.full.AKM := lrhwage-lrhwage.pred]
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(resid.full.AKM=mean(resid.full.AKM)), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# mean workerFE
# already calculated earlier

# mean age
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_age = mean(age)), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# mean age profile
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(age_profile=  mean(age.profile)), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# residual already added (but that should be zero because of firm year FEs)

#### Inmovers

# number of inmovers
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(workers_inmovers = sum(inmove_mover)+sum(inmove_NA)), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean log wage
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_lrhwage_in = mean(lrhwage[inmove_mover|inmove_NA])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean workerFE
tcBEIDs_ana[, workerFE_in := NULL]
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(workerFE_in = mean(worker_FE[inmove_mover|inmove_NA])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean age
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_age_in = mean(age[inmove_mover|inmove_NA])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# mean age profile
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(age_profile_in= mean(age.profile[inmove_mover|inmove_NA])), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# residual
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(resid_in = mean(resid.full.AKM[inmove_mover|inmove_NA])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)


#### Outmovers

# number of Outmovers
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(workers_outmovers = sum(outmove_mover)+sum(outmove_NA)), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)


# mean log wage
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_lrhwage_out = mean(lrhwage[outmove_mover|outmove_NA])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean workerFE
tcBEIDs_ana[, workerFE_out := NULL]
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(workerFE_out = mean(worker_FE[outmove_mover|outmove_NA])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean age
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_age_out = mean(age[outmove_mover|outmove_NA])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# mean age profile
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(age_profile_out=  mean(age.profile[outmove_mover|outmove_NA])), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# residual
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(resid_out = mean(resid.full.AKM[outmove_mover|outmove_NA])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)


## Now lag everything, so that current period describes those that left in between t and t-1
setorderv(tcBEIDs_ana, c("tcBEID", "year"))

# number Outmovers
tcBEIDs_ana[, workers_outmovers.lag := shift(workers_outmovers, n=1, type="lag"), by=tcBEID]


# wage 
tcBEIDs_ana[, mean_lrhwage_out.lag := shift(mean_lrhwage_out, n=1, type="lag"), by=tcBEID]

# workerFE
tcBEIDs_ana[, workerFE_out.lag := shift(workerFE_out, n=1, type="lag"), by=tcBEID]

# firmFE
tcBEIDs_ana[, firm_FE.lag := shift(firm_FE, n=1, type="lag"), by=tcBEID]

# mean age
tcBEIDs_ana[, mean_age_out.lag := shift(mean_age_out, n=1, type="lag"), by=tcBEID]

# mean age profile 
tcBEIDs_ana[, age_profile_out.lag := shift(age_profile_out, n=1, type="lag"), by=tcBEID]

# residual
tcBEIDs_ana[, resid_out.lag := shift(resid_out, n=1, type="lag"), by=tcBEID]


#### Stayers	 
# stayer up from least time_takeover==-3
RINPs_ana[, stayer.takeover1 := (all(diff(year)==1) & any(time_takeover==-3) & any(time_takeover==3)), by=c("RINP", "tcBEID")]


# mean log wage
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_lrhwage_stayer1 = mean(lrhwage[stayer.takeover1])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean workerFE 
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(workerFE_stayer = mean(worker_FE[stayer.takeover1])), by=c("tcBEID", "year")],
                     by=c("tcBEID", "year"), all.x=TRUE)

# mean age (---> should not be necessary)
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(mean_age_stayer = mean(age[stayer.takeover1])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# mean age profile
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(age_profile_stayer= mean(age.profile[stayer.takeover1])), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# residual
tcBEIDs_ana <- merge(tcBEIDs_ana, RINPs_ana[, .(resid_stayer = mean(resid.full.AKM[stayer.takeover1])), by=c("year","tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

#### percentile cutoffs ----

# find within firm 0.25 & 0.5 percentile cutoffs at time_takeover==-1
RINPs_ana[, c("cutoff.q1", "cutoff.q2", "cutoff.q3") := list(quantile(worker_FE[time_takeover==-1], p=0.25), 
                                              quantile(worker_FE[time_takeover==-1], p=0.5),
                                              quantile(worker_FE[time_takeover==-1], p=0.75)), 
          by=c("tcBEID")]

# per time_takeover find within firm share of workerFE's below cutoff from time_takeover==-1
RINPs_ana[, c("number.workerFE.q1", "number.workerFE.q2", "number.workerFE.q3", "number.workerFE.q4") := list(sum(worker_FE<cutoff.q1), 
                                                                              sum(worker_FE>=cutoff.q1 & worker_FE<cutoff.q2),
                                                                              sum(worker_FE>=cutoff.q2 & worker_FE<cutoff.q3),
                                                                              sum(worker_FE>=cutoff.q3)), by=c("tcBEID", "time_takeover")]

# aggregate and merge with tcBEIDs_ana
tcBEIDs_ana <- merge(tcBEIDs_ana, 
                     RINPs_ana[, .(number.workerFE.q1=number.workerFE.q1[1], number.workerFE.q2=number.workerFE.q2[1],
                                   number.workerFE.q3=number.workerFE.q3[1], number.workerFE.q4=number.workerFE.q4[1]), by=c("tcBEID", "time_takeover")],
                     by=c("tcBEID", "time_takeover"), all.x=TRUE)
tcBEIDs_ana[, c("ratio.workerFE.q1", "ratio.workerFE.q2", "ratio.workerFE.q3", "ratio.workerFE.q4") := list(number.workerFE.q1/workers, number.workerFE.q2/workers,
                                                                              number.workerFE.q3/workers, number.workerFE.q4/workers)]

#### identify cohorts & other vars ----
tcBEIDs_ana[, acq.cohort := fifelse(treatment_group2[1]==TRUE,year[which(time_takeover==0)],0), by=tcBEID]
tcBEIDs_ana[, all.cohort := year[which(time_takeover==0)], by=tcBEID]
tcBEIDs_ana[, matching_group.year := paste0(matching_group, "-", year)]


#### Some other vars to add ----
tcBEIDs_ana[, percentage_workerFE_q1 := (number.workerFE.q1/workers)*100]
tcBEIDs_ana[, percentage_workerFE_q34 := ((number.workerFE.q3+number.workerFE.q4)/workers)*100]
tcBEIDs_ana[, NA_before := NA_before.y]
tcBEIDs_ana[, c("NA_before.x", "NA_before.y") := NULL]
tcBEIDs_ana[is.na(pre_knowledge), pre_knowledge:=FALSE]
tcBEIDs_ana[, pre_knowledge := as.logical(max(pre_knowledge)), by=matching_group]

#### time to treatment ----
# set time to treatment for controls to -1000
tcBEIDs_ana[, time_to_treatment := fifelse(treatment_group2, time_takeover, -1000)]

# subset to time frame
tcBEIDs_ana <- tcBEIDs_ana[time_takeover %in% -3:3,]

# save result
saveRDS(tcBEIDs_ana, paste0(map_output_here,"tcBEIDs_analysis_z_",h,".rds"), compress=TRUE)

gc()
}

rm(fit, overview, tcBEIDs_year_ana, RINPs_ana, RINPs_year, tcBEIDs_ana)
gc()