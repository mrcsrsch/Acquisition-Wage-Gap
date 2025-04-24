  ##################################################################################
  # Create numeric ID datasets
  ##################################################################################
  
  #### required packages ----
  if (!require("fixest")) install.packages("fixest"); library("fixest")
  
  
  #### load data ----
  # Data set for analysis: PSM matched dataset 
  tcBEIDs_ana <- readRDS(paste0(map_data,"step8/tcBEIDs_analysis_z_",1,".rds"))
  
  # Data set before PSM matching
  tcBEIDs_full <- readRDS(paste0(map_data,"step7/tcBEIDs_year_matching.rds"))
  
  # load full employer employee network
  RINPs_year <- readRDS(paste0(map_data,"step6/yeartcBEID/tcBEIDyear_AKM.rds"))
  
  #### output map ----- 
  if (!dir.exists(paste0(map_data, "step9/"))) dir.create(paste0(map_output, "step9/"))
  map_out_here <- paste0(map_data, "step9/")

  ##################################################################################
  ##################################################################################
  
  # assign numeric identifiers 
  # RINP
  RINPs_year[, RINP_n := match(RINP, RINP)]
  # tcBEID
  RINPs_year[, tcBEID_n := match(tcBEID, tcBEID)]
  # tcBEID_year
  RINPs_year[, tcBEID_year := paste0(tcBEID, "-", year)]
  RINPs_year[, tcBEID_year_n := match(tcBEID_year, tcBEID_year)]
  
  # extract translation tables
  trans_tcBEID <- unique(RINPs_year[, c("tcBEID", "tcBEID_n")])
  trans_tcBEIDyear <- unique(RINPs_year[, c("tcBEID_year", "tcBEID_year_n", "tcBEID", "year")])
  trans_RINP <-  unique(RINPs_year[, c("RINP", "RINP_n")])
  
  # setname in RINPs_year
  RINPs_year[, RINP :=NULL]
  setnames(RINPs_year, "RINP_n", "RINP")
  
  # add numeric ids to tcBEIDs_ana
  tcBEIDs_ana <- merge(tcBEIDs_ana, trans_tcBEID, by="tcBEID")
  tcBEIDs_ana <- merge(tcBEIDs_ana, trans_tcBEIDyear[, c("tcBEID_year_n", "tcBEID", "year")], by=c("tcBEID", "year"))
  tcBEIDs_ana[, tcBEID := NULL]
  setnames(tcBEIDs_ana, c("tcBEID_n", "tcBEID_year_n"), c("tcBEID", "tcBEID_year"))
  
  # add numeric ids to tcBEIDs_full
  tcBEIDs_full <- merge(tcBEIDs_full, trans_tcBEID, by="tcBEID")
  tcBEIDs_full <- merge(tcBEIDs_full, trans_tcBEIDyear[, c("tcBEID_year_n", "tcBEID", "year")], by=c("tcBEID", "year"))
  tcBEIDs_full[, tcBEID := NULL]
  setnames(tcBEIDs_full, c("tcBEID_n", "tcBEID_year_n"), c("tcBEID", "tcBEID_year"))
  
  # clean up datasets
  RINPs_year[, c("tcBEID", "tcBEID_year") := NULL]
  setnames(RINPs_year, c("tcBEID_n", "tcBEID_year_n"), c("tcBEID", "tcBEID_year"))
  RINPs_year[ , c("SBEID", "obs_firm_FE", "obs_worker_FE", "cont_observed_firm", "years_panel_firm", "entry_year_firm", "observed_2018") := NULL]
  
  tcBEIDs_ana[, c("active_since", "active_until", "log_wage", "OG_SBEIDs", "pre_knowledge", "stratum", "mean_age",
                  "workers_inmovers", "mean_lrhwage_in", "workerFE_in", "mean_age_in", "age_profile_in",   "resid_in", "workers_outmovers",   
   "mean_lrhwage_out", "workerFE_out",     "mean_age_out",     "age_profile_out",  "resid_out",       
   "workers_outmovers.lag",   "mean_lrhwage_out.lag",    "workerFE_out.lag", "firm_FE.lag",      "mean_age_out.lag",       
   "age_profile_out.lag",     "resid_out.lag",    "mean_lrhwage_stayer1",    "workerFE_stayer",  "mean_age_stayer", 
   "age_profile_stayer",      "resid_stayer",     "number.workerFE.q1",     "number.workerFE.q2",      "number.workerFE.q3",     
   "number.workerFE.q4",     "ratio.workerFE.q1",       "ratio.workerFE.q2",       "ratio.workerFE.q3",       "ratio.workerFE.q4",    
   "acq.cohort",      "all.cohort",              "percentage_workerFE_q1",  "percentage_workerFE_q34",
   "NA_before", "matching_group.year") := NULL]
  
  tcBEIDs_full[, c("rhwage_mover",                    "rhwage_stayer",                   "resid_1",                        
                            "ins_mover",                       "ins_NA",                         
   "outs_mover",                      "out_NA",                          "workerFE_mover",                 
   "workerFE_stayer",                 "workerFE_moverin",                "workerFE_moverout",              
   "workerFE_NAin",                   "workerFE_NAout",                  "workerFE_in",                    
   "workerFE_out",                                                          
   "cont_observed_firm",              "years_panel_firm",                "entry_year_firm",                
   "observed_until2018",              "workers",                         "only_fake",                      
                                                             
   "year_MNE_OG",                                                                
                                          
   "min_age",                                              
              "pre_knowledge",                  
   "foreign_acq.lead",                "NA_before",                       "active_since",                   
   "active_until",                                  "foreign_acq.lead2",              
  "log_wage",                        "log_employment",                  "workerFE_2",                     
  "firm_FE_2",                       "workerFE_var",                    "workerFE.lag1",                  
  "workerFE.lag2",                   "firm_FE.lag1",                    "firm_FE.lag2",                   
  "log_wage.lag1",                   "log_wage.lag2",                   "mean_lrhwage.lag1",              
  "mean_lrhwage.lag2",               "log_employment.lag1",             "log_employment.lag2",            
  "workerFE_var.lag1",               "workerFE_var.lag2",               "firm_FE.change1",                
  "workerFE.change1",                "firm_FE.change2",                 "workerFE.change2",               
  "log_wage.change",                 "log_wage.change2",                "mean_lrhwage.change",            
  "mean_lrhwage.change2",            "log_employment.change",           "log_employment.change2",         
  "mean_lrhwage.2change",            "log_wage.2change",                "log_employment.2change",         
  "firm_FE.2change",                 "workerFE.2change",                "log_exports",                    
  "log_imports",                     "log_reexports",                   "import",                         
  "export",                          "reexport",                        "log_importcountries",            
  "log_exportcountries",             "log_reexportcountries",                              
  "log_sales",                       "log_sales_worker",                "log_sales.change",               
  "log_laborproductivity",           "log_meanage",                     "share_females") := NULL]                  
  
  # add age profile and year fe's to RINPs_year
  # load coefs
  fit <- readRDS(paste0(map_data,"step6/yeartcBEID/fit_age_year.rds"))
  fit <- list(coefficients=fit$coefficients)
  
  # calculate age profile
  RINPs_year[, age.profile := 
               (age-40)*fit$coefficients[grepl("age_1", names(fit$coefficients))]  +
               (age-40)^2*fit$coefficients[grepl("age_2", names(fit$coefficients))]  +
               (age-40)^3*fit$coefficients[grepl("age_3", names(fit$coefficients))]]
  
  # add year fixed effects
  for (years in 2007:2018){
    RINPs_year[year==years, year_fe := fit$coefficients[grepl(paste0(years), names(fit$coefficients))]]
  }
  rm(fit)
  
  #### save result  #####
  saveRDS(tcBEIDs_full, paste0(map_output_here,"tcBEIDs_year_numeric.rds"), compress=TRUE)
  saveRDS(tcBEIDs_ana, paste0(map_output_here,"tcBEIDs_ana_numeric.rds"), compress=TRUE)
  saveRDS(RINPs_year, paste0(map_output_here,"RINPs_year_numeric.rds"), compress=TRUE)
  saveRDS(trans_tcBEID, paste0(map_output_here,"trans_tcBEID.rds"), compress=TRUE)
  saveRDS(trans_tcBEIDyear, paste0(map_output_here,"trans_tcBEIDyear.rds"), compress=TRUE)
  saveRDS(trans_RINP, paste0(map_output_here,"trans_RINP.rds"), compress=TRUE)
