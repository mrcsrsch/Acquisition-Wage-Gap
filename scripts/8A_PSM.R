##################################################################################
# This script implements the matching strategy
##################################################################################
### required packages #### 
if (!require("ggplot2"))  install.packages("ggplot2"); library("ggplot2")
if (!require("xtable"))  install.packages("xtable"); library("xtable")

### output dir ####
if (!dir.exists(paste0(map_data, "step8/"))) dir.create(paste0(map_data, "step8/"))
map_output_here <- paste0(map_data, "step8/")

if (!dir.exists(paste0(map_output, "PSM/"))) dir.create(paste0(map_data, "PSM/"))

### load data ####
tcBEIDs_year_ana <- readRDS(paste0(map_data,"step7/tcBEIDs_year_matching.rds"))
##################################################################################
##################################################################################
# only need some of these variables
tcBEIDs_year_ana <- tcBEIDs_year_ana[, c("tcBEID", "year", "resid_1", "firm_FE", "workerFE_mover", "workerFE_stayer",  "workerFE_moverin", "workerFE_moverout", "workerFE_NAin",     
                                         "workerFE_NAout", "workerFE_in", "workerFE_out", "workerFE",                       
                                         "workerFE_sd", "observed_until2018", "workers",  "nace",                     
                                         "nace21", "BULA_DOCHTERS", "bui_mul", "year_MNE_OG",                    
                                         "OG_BEID", "OG_SBEIDs", "foreign_acq.lead2",                     
                                         "brownfield_bui",  "nace_2digit", "treatment_group",                
                                         "time_takeover", "pre_knowledge",  "foreign_acq.lead",               
                                         "NA_before", "active_since",   "active_until",   "potential_control",              
                                         "log_wage", "workerFE.lag1",  "workerFE.lag2",  "firm_FE.lag1",  
                                         "firm_FE.lag2", "log_wage.lag1",  "log_wage.lag2",  "firm_FE.change1",                
                                         "workerFE.change1",  "firm_FE.change2",  "workerFE.change2", "log_wage.change2",               
                                         "log_wage.change", "log_employment", "log_employment.lag1",            
                                         "log_employment.lag2", "log_employment.change", "log_employment.change2", "log_exports",   
                                         "log_firmage", "log_meanage", "share_females", "mean_age",
                                         "workerFE_var", "workerFE_var.lag1", "workerFE_var.lag2",
                                         "workerFE_2", "firm_FE_2", "log_wage.2change", "log_employment.2change",
                                         "firm_FE.2change", "workerFE.2change",
                                         "mean_lrhwage.2change", "mean_lrhwage.lag1", "mean_lrhwage.lag2", "mean_lrhwage.change", "mean_lrhwage")]

#### add stayers wages and mean age profile
# load RINPs_year
RINPs_year <- readRDS(paste0(map_data,"step7/RINPs_year.rds"))

### Add age profile ---
# load coefficients
fit <- readRDS(paste0(map_data,"step6/yeartcBEID/fit_age_year.rds"))
fit2 <- fit$coefficients
rm(fit)
fit <- list(coefficients=fit2)
rm(fit2)
gc()

# add age profile to RINPs_year
# add coefficients
RINPs_year[, age_1.coef := fit$coefficients[grepl("age_1", names(fit$coefficients))]]
RINPs_year[, age_2.coef := fit$coefficients[grepl("age_2", names(fit$coefficients))]]
RINPs_year[, age_3.coef := fit$coefficients[grepl("age_3", names(fit$coefficients))]]

# add age profile 
RINPs_year[, age.profile := 
             (age-40)*age_1.coef  +
             (age-40)^2*age_2.coef  +
             (age-40)^3*age_3.coef, by=age]

# add mean age profile and var of wages to tcBEIDs_year_ana
tcBEIDs_year_ana <- merge(tcBEIDs_year_ana, RINPs_year[, .(mean_age_profile=  mean(age.profile), lrhwage_var = var(lrhwage)), by=c("year", "tcBEID")], by=c("tcBEID", "year"), all.x=TRUE)

# clean up
rm(RINPs_year, fit)
gc()

#### Create a list to store matching result ----
overview <- list()

# Main analysis
overview[[1]] <- list(predictors=c("mean_lrhwage", "mean_lrhwage.change", "mean_lrhwage.2change", 
                                   "log_employment", "log_employment.change", "log_employment.2change", 
                                   "firm_FE", "firm_FE.change1", "firm_FE.2change",  
                                   "workerFE", "workerFE.change1", "workerFE.2change",
                                   "workerFE_var",
                                   "log_firmage", "log_exports")) 
overview[[1]]$plots <- TRUE # create density and balance plots?
overview[[1]]$nace2 <- TRUE # first put into nace 2 digit strata? 
overview[[1]]$collapse <- TRUE # and then collapse into nace 1 digit strata if any treatment outside common support?
overview[[1]]$time_takeover <- -1 # match at which year? 
overview[[1]]$dep.var <- "foreign_acq.lead" # dependent variable for propensity scores

# loop over the matching strategies
for (z in 1:length(overview)){ 
  
  cat(paste(Sys.time(), "Starting with:", z), "\n")
  
  
  tcBEIDs_year <- copy(tcBEIDs_year_ana)
  
  #### Propensity score matching vars ---- 
  # define formula
  dependent1 <- overview[[z]]$dep.var
  dependent2 <- "treatment_group"
  predictors <- overview[[z]]$predictors
  
  #### Subset to treatment and potential control in strata ----
  # only keep relevant years
  tt <- tcBEIDs_year[time_takeover==overview[[z]]$time_takeover, unique(year)]
  tcBEIDs_year <- tcBEIDs_year[year %in% tt, ]
  rm(tt)
  
  # if overview[[z]]$nace2==TRUE sort into nace2-digit strata first, otherwise into nace1-digit strata right away
  # sort observations into year-nace-2digit-strata
  if (overview[[z]]$nace2==TRUE){
    tcBEIDs_year[, stratum := paste0(year,"-",nace_2digit)]
    
  } else { 
    tcBEIDs_year[, stratum := paste0(year,"-",nace21)]
  }
  obs <- tcBEIDs_year[, .(moment=1,
                          type="nace2-year", 
                          strata=uniqueN(stratum),
                          foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                          treated=uniqueN(tcBEID[treatment_group]), 
                          pcontrol=uniqueN(tcBEID[potential_control]), 
                          N=.N)]
  
  # only keep foreign acquired firms in matching year
  tcBEIDs_year[, keep := TRUE]
  IDs <- tcBEIDs_year[eval(as.symbol(dependent1))==TRUE, unique(tcBEID)]
  tcBEIDs_year[tcBEID %in% IDs, keep := FALSE]
  tcBEIDs_year[tcBEID %in% IDs & eval(as.symbol(dependent1))==TRUE, keep := TRUE]
  tcBEIDs_year <- tcBEIDs_year[keep==TRUE, !c("keep")]
  rm(IDs)
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=2,
                                         type="nace2-year", 
                                         strata=uniqueN(stratum),
                                         foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[potential_control]), 
                                         N=.N)]))
  
  # only keep treatment and potential control firms
  tcBEIDs_year <- tcBEIDs_year[treatment_group==TRUE | potential_control==TRUE,]
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=3,
                                         type="nace2-year", 
                                         strata=uniqueN(stratum),
                                         foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[potential_control]), 
                                         N=.N)]))
  
  # only keep complete cases (all predictors observed)
  tcBEIDs_year <- na.omit(tcBEIDs_year, cols=c(dependent1, dependent2, predictors, 
                                               "year",
                                               "nace21",
                                               "nace_2digit"))
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=4,
                                         type="nace2-year", 
                                         strata=uniqueN(stratum),
                                         foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[potential_control]), 
                                         N=.N)]))
  
  # only keep strata with at least one treated firm
  tcBEIDs_year[, keep := any(treatment_group==TRUE), by=stratum]
  tcBEIDs_year <- tcBEIDs_year[keep==TRUE, !c("keep")]
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=5,
                                         type="nace2-year", 
                                         strata=uniqueN(stratum),
                                         foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[potential_control]), 
                                         N=.N)]))
  
  # only keep strata with at a treated/potential control ratio of <=1
  tcBEIDs_year[, keep := (sum(potential_control)>=sum(treatment_group)), by=stratum]
  tcBEIDs_year <- tcBEIDs_year[keep==TRUE, !c("keep")]
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=8,
                                         type="nace2-year", 
                                         strata=uniqueN(stratum),
                                         foreign=uniqueN(tcBEID[eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[potential_control]), 
                                         N=.N)]))
  
  #### Estimate propensity scores (first) ----
  # formula
  form <- as.formula(paste0(dependent1, "~", paste0(predictors, collapse = "+")))
  
  strata <- tcBEIDs_year[, unique(stratum)]
  for (i in strata){
    #cat(paste(Sys.time(), "Stratum", which(strata==i), "of", length(strata)), "\n")
    # estimate logit model
    prop_model <- glm(form, family = binomial(), model=FALSE, data=tcBEIDs_year[stratum==i,], control = list(maxit=10000))
    # predict propensity scores
    tcBEIDs_year[stratum==i, propensity_score := prop_model$linear.predictors] #predict(prop_model)
  }
  rm(i, strata, prop_model)
  
  #### Apply nearest neighbour matching within each stratum (first) ----

  # extract propensity scores
  treated <- tcBEIDs_year[treatment_group==TRUE, propensity_score]
  control <- tcBEIDs_year[potential_control==TRUE, propensity_score]
  # create matrix of absolute differences between treated and control
  mm <- abs(outer(treated, control, FUN="-"))
  rm(treated, control)

  # extract strata
  treated <- tcBEIDs_year[treatment_group==TRUE, stratum]
  control <- tcBEIDs_year[potential_control==TRUE, stratum]
  # find overlap matrix ss
  ss <- outer(treated, control, FUN="==") 
  rm(treated, control)
  
  # subset mm to pairs in same strata, i.e. remove across strata differences
  mm[!ss] <- NA
  rm(ss)
  gc()
  
  # store tcBEIDs vector for controls
  control <- tcBEIDs_year[potential_control==TRUE, tcBEID]
  
  # store positions in data.table
  treated_pos <- tcBEIDs_year[treatment_group==TRUE, which=TRUE]
  control_pos <- tcBEIDs_year[potential_control==TRUE, which=TRUE]
  

  timer <- Sys.time()
  paste0(timer, "\n")
  # loop from here
  g <- 0
  match.matrix <- matrix(NA, nrow=nrow(mm), ncol=3)
  repeat{
    g <- g+1
    cat(paste(g, "   "))
    # find minimum position in mm
    pos <- arrayInd(which.min(mm), dim(mm)) #  which(mm==min(mm, na.rm=TRUE), arr.ind=TRUE)
    
    # stop criterion: no further minimum
    if (length(pos)==0) break
    
    # add matching_group to match.matrix
    match.matrix[g,] <- c(treated_pos[pos[1]], control_pos[pos[2]], g)
    
    # remove treated firm and ALL columns of control firm from mm (set them to NA)
    mm[pos[1],] <- NA
    ## need to find columns of control in control vector (holds tcBEIDs of controls now)
    mm[, which(control==control[pos[2]])] <- NA
  }
  timer <- Sys.time()-timer
  cat("Timer: ", timer, " mins \n") #around 25 mins
  
  # remove NAs from match.matrix
  match.matrix <- match.matrix[!is.na(match.matrix[,3]),]
  
  # add matching groups to tcBEIDs_year
  tcBEIDs_year[match.matrix[,1], matching_group:=match.matrix[,3]]
  tcBEIDs_year[match.matrix[,2], matching_group:=match.matrix[,3]]
  
  # clean up
  # first save match.matrix in case something goes wrong
  match.matrix.twodigit <- match.matrix
  rm(match.matrix, timer, pos, g, control, treated_pos, control_pos, mm)
  
  
  # Caliber matching: (Austin 2011) remove propensity scores that are larger than 0.2*sd(within stratum propensity scores)
  
  # standard dev. per stratum
  tcBEIDs_year[, sd_propensity_scores := sd(propensity_score), by=stratum]
  
  # find undesired groups
  tcBEIDs_year[!is.na(matching_group), keep := abs(diff(propensity_score))<=(0.2*sd_propensity_scores[1]), by=matching_group]
  
  # delete these 
  tcBEIDs_year[keep==FALSE, matching_group:=NA]
  tcBEIDs_year[, keep:=NULL]
  
  # store observations per matched stratum
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=9,
                                         type="matched-nace2-year", 
                                         strata=uniqueN(stratum[!is.na(matching_group) & (substr(stratum, 6,6) %in% as.character(seq(0,9)))]),
                                         foreign=uniqueN(tcBEID[!is.na(matching_group) & eval(as.symbol(dependent1)) & (substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                         treated=uniqueN(tcBEID[!is.na(matching_group) & treatment_group & (substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                         pcontrol=uniqueN(tcBEID[!is.na(matching_group) & potential_control & (substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                         N=sum(!is.na(matching_group) & (substr(stratum, 6,6) %in% as.character(seq(0,9)))))]))
  
  ##### Find treatment and control firms without a matching_group ######
  
  # set unmatched to false for all firms
  tcBEIDs_year[, unmatched:=FALSE]
  
  # find unmatched treated firms
  tcBEIDs_year[treatment_group==TRUE & is.na(matching_group), unmatched:=TRUE]
  
  # find unmatched potential control firms
  tcBEIDs_year[potential_control==TRUE, unmatched:=!any(!is.na(matching_group)) , by=tcBEID]
  
  # reassign matching group for matched firms (and simply save it somewhere else)
  tcBEIDs_year[unmatched==FALSE & !is.na(matching_group), nace2_matching_group := as.numeric(paste0("220", matching_group))]
  
  # delete old matching group
  tcBEIDs_year[, matching_group := NULL]
  
  
  #### Collapse unmatched strata ----
  if (overview[[z]]$collapse==TRUE){
    # sort unmatched firms in nace-1 strata
    tcBEIDs_year[unmatched==TRUE, stratum := paste0(year,"-",nace21)]
    
    # only keep strata with at least one treated firm (set unmatched to FALSE for others)
    tcBEIDs_year[unmatched==TRUE, unmatched := any(treatment_group==TRUE), by=stratum]
    
    # only keep strata with at a treated/potential control ratio of <=1 (set unmatched to FALSE for others)
    tcBEIDs_year[unmatched==TRUE, unmatched := (sum(potential_control)>=sum(treatment_group)), by=stratum]
   # tcBEIDs_year <- tcBEIDs_year[keep==TRUE, !c("keep")]
    
    # store unmatched observations
    obs <- rbindlist(list(obs,
                          tcBEIDs_year[, .(moment=10,
                                           type="unmatched-nace1-year", 
                                           strata=uniqueN(stratum[!(substr(stratum, 6,6) %in% as.character(seq(0,9)))]),
                                           foreign=uniqueN(tcBEID[eval(as.symbol(dependent1)) & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           treated=uniqueN(tcBEID[treatment_group & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           pcontrol=uniqueN(tcBEID[potential_control & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           N=sum(!(substr(stratum, 6,6) %in% as.character(seq(0,9)))))]))
    
    
    #### Estimate propensity scores (second) ----
    # formula
    form <- as.formula(paste0(dependent1, "~", paste0(predictors, collapse = "+")))
    
    # only do this for unmatched strata (i.e. nace1 strata)
    strata <- tcBEIDs_year[unmatched==TRUE, unique(stratum)]
    for (i in strata){
      #cat(paste(Sys.time(), "Stratum", which(strata==i), "of", length(strata)), "\n")
      # estimate logit model
      prop_model <- glm(form, family = binomial(), model=FALSE, data=tcBEIDs_year[stratum==i,], control = list(maxit=10000))
      # predict propensity scores
      tcBEIDs_year[stratum==i, propensity_score := prop_model$linear.predictors] #predict(prop_model)
    }
    rm(i, strata, prop_model)
    
    #### Apply nearest neighbour matching within each stratum (second) ----
    
    ### This is the same as above, just running on the unmatched==TRUE subset (rows in data stay constant)
    
    # extract propensity scores
    treated <- tcBEIDs_year[treatment_group==TRUE & unmatched==TRUE, propensity_score]
    control <- tcBEIDs_year[potential_control==TRUE & unmatched==TRUE, propensity_score]
    # create matrix of absolute differences between treated and control
    mm <- abs(outer(treated, control, FUN="-"))
    rm(treated, control)
    
    # extract strata
    treated <- tcBEIDs_year[treatment_group==TRUE & unmatched==TRUE, stratum]
    control <- tcBEIDs_year[potential_control==TRUE & unmatched==TRUE, stratum]
    # find overlap matrix ss
    ss <- outer(treated, control, FUN="==") 
    rm(treated, control)
    
    # subset mm to pairs in same strata, i.e. remove across strata differences
    mm[!ss] <- NA
    rm(ss)
    gc()
    
    # store tcBEIDs vector for controls
    control <- tcBEIDs_year[potential_control==TRUE & unmatched==TRUE, tcBEID]
    
    # store positions in data.table
    treated_pos <- tcBEIDs_year[treatment_group==TRUE & unmatched==TRUE, which=TRUE]
    control_pos <- tcBEIDs_year[potential_control==TRUE & unmatched==TRUE, which=TRUE]
    
    
    timer <- Sys.time()
    paste0(timer, "\n")
    # loop from here
    g <- 0
    match.matrix <- matrix(NA, nrow=nrow(mm), ncol=3)
    repeat{
      g <- g+1
      cat(paste(g, "   "))
      # find minimum position in mm
      pos <- arrayInd(which.min(mm), dim(mm)) #  which(mm==min(mm, na.rm=TRUE), arr.ind=TRUE)
      
      # stop criterion: no further minimum
      if (length(pos)==0) break
      
      # add matching_group to match.matrix
      match.matrix[g,] <- c(treated_pos[pos[1]], control_pos[pos[2]], g)
      
      # remove treated firm and ALL columns of control firm from mm (set them to NA)
      mm[pos[1],] <- NA
      ## need to find columns of control in control vector (holds tcBEIDs of controls now)
      mm[, which(control==control[pos[2]])] <- NA
    }
    timer <- Sys.time()-timer
    cat("Timer: ", timer, " mins \n") #1.2 mins
    
    # remove NAs from match.matrix
    match.matrix <- match.matrix[!is.na(match.matrix[,3]),]
    
    # add matching groups to tcBEIDs_year
    tcBEIDs_year[match.matrix[,1], matching_group:=match.matrix[,3]]
    tcBEIDs_year[match.matrix[,2], matching_group:=match.matrix[,3]]
    
    # clean up
    # first save match.matrix in case something goes wrong
    match.matrix.onedigit <- match.matrix
    rm(match.matrix, timer, pos, g, control, treated_pos, control_pos, mm)
    
    
    # Caliber matching: (Austin 2011) remove propensity scores that are larger than 0.2*sd(within stratum propensity scores)
    
    # standard dev. per stratum
    tcBEIDs_year[unmatched==TRUE, sd_propensity_scores := sd(propensity_score), by=stratum]
    
    # find undesired groups
    tcBEIDs_year[!is.na(matching_group) & unmatched==TRUE, keep := abs(diff(propensity_score))<=(0.2*sd_propensity_scores[1]), by=matching_group]
    
    # delete these 
    tcBEIDs_year[keep==FALSE, matching_group:=NA]
    tcBEIDs_year[, keep:=NULL]
    
    # save matched observations
    obs <- rbindlist(list(obs,
                          tcBEIDs_year[, .(moment=11,
                                           type="matched-nace1-year", 
                                           strata=uniqueN(stratum[!is.na(matching_group) & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]),
                                           foreign=uniqueN(tcBEID[!is.na(matching_group) & eval(as.symbol(dependent1)) & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           treated=uniqueN(tcBEID[!is.na(matching_group) & treatment_group & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           pcontrol=uniqueN(tcBEID[!is.na(matching_group) & potential_control & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))]), 
                                           N=sum(!is.na(matching_group) & !(substr(stratum, 6,6) %in% as.character(seq(0,9)))))]))
    
    
    # change matching group to reflect nace-1 strata
    tcBEIDs_year[!is.na(matching_group), matching_group := as.numeric(paste0("110", matching_group))]
    
  }
  
  # combine matching groups
  tcBEIDs_year[!is.na(nace2_matching_group), matching_group := nace2_matching_group]
  tcBEIDs_year[, nace2_matching_group:=NULL]
  
  obs <- rbindlist(list(obs,
                        tcBEIDs_year[, .(moment=11,
                                         type="matched-full", 
                                         strata=uniqueN(stratum[!is.na(matching_group)]),
                                         foreign=uniqueN(tcBEID[!is.na(matching_group) & eval(as.symbol(dependent1))]), 
                                         treated=uniqueN(tcBEID[!is.na(matching_group) & treatment_group]), 
                                         pcontrol=uniqueN(tcBEID[!is.na(matching_group) & potential_control]), 
                                         N=.N)]))
  obs
  overview[[z]]$obs <- obs
  rm(obs)
  
  #### Covariate balance: Before vs. after matching ----

  # assign weight of one for each matched firm
  tcBEIDs_year[, matched := as.integer(!is.na(matching_group))]
  
  
  
  #### Density of wage components ----
  # Only do this step if overview[[z]]$plots==TRUE
  
  if (overview[[z]]$plots==TRUE){
    # Before matching
    
    # mean ln wage
    ggplot(data=tcBEIDs_year[, .(mean_lrhwage=mean_lrhwage, type=fifelse(treatment_group, "Foreign Acquistions", "Potential Controls"))]) + 
      geom_density(aes(x=mean_lrhwage, colour=type), size=0.8) +
      xlab("Mean ln wage") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_lrhwage_density_before_", z, ".pdf"), plot=last_plot())
    
    # Firm FE
    ggplot(data=tcBEIDs_year[, .(firm_FE=firm_FE, type=fifelse(treatment_group, "Foreign Acquistions", "Potential Controls"))]) + 
      geom_density(aes(x=firm_FE, colour=type), size=0.8) +
      xlab("Firm fixed effect") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_FirmFE_density_before_", z, ".pdf"), plot=last_plot())
    
    # Mean Worker FE
    ggplot(data=tcBEIDs_year[, .(workerFE=workerFE, type=fifelse(treatment_group, "Foreign Acquistions", "Potential Controls"))]) + 
      geom_density(aes(x=workerFE, colour=type), size=0.8) +
      xlab("Mean worker fixed effect") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_WorkerFE_density_before_", z, ".pdf"), plot=last_plot())
    
    # mean wage-age profile
    ggplot(data=tcBEIDs_year[, .(mean_age_profile=mean_age_profile, type=fifelse(treatment_group, "Foreign Acquistions", "Potential Controls"))]) + 
      geom_density(aes(x=mean_age_profile, colour=type), size=0.8) +
      xlab("Mean wage-age profile") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_ageprofile_density_before_", z, ".pdf"), plot=last_plot())
    
    
    # After matching
    # mean ln wage
    ggplot(data=tcBEIDs_year[matched==TRUE, .(mean_lrhwage=mean_lrhwage, type=fifelse(treatment_group, "Foreign Acquistions", "Matched Controls"))]) + 
      geom_density(aes(x=mean_lrhwage, colour=type), size=0.8) +
      xlab("Mean ln wage") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_lrhwage_density_after_", z, ".pdf"), plot=last_plot())
    
    
    # Firm FE
    ggplot(data=tcBEIDs_year[matched==TRUE, .(firm_FE=firm_FE, type=fifelse(treatment_group, "Foreign Acquistions", "Matched Controls"))]) + 
      geom_density(aes(x=firm_FE, colour=type), size=1) +
      xlab("Firm Fixed Effect") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_FirmFE_density_after_", z, ".pdf"), plot=last_plot())
    
    
    # Worker FE
    ggplot(data=tcBEIDs_year[matched==TRUE, .(workerFE=workerFE, type=fifelse(treatment_group, "Foreign Acquistions", "Matched Controls"))]) + 
      geom_density(aes(x=workerFE, colour=type), size=0.8) +
      xlab("Mean Worker Fixed Effect") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_WorkerFE_density_after_", z, ".pdf"), plot=last_plot())
    
    # mean wage-age profile
    ggplot(data=tcBEIDs_year[matched==TRUE, .(mean_age_profile=mean_age_profile, type=fifelse(treatment_group, "Foreign Acquistions", "Matched Controls"))]) + 
      geom_density(aes(x=mean_age_profile, colour=type), size=0.8) +
      xlab("Mean wage-age profile") + ylab("Density") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
            legend.position = "bottom",
            legend.title = element_blank())
    ggsave(paste0(map_output,"PSM/plot_mean_ageprofile_density_after_", z, ".pdf"), plot=last_plot())
  }
  
  #### Calculate standardized mean difference per stratum (=matching_group) by hand ----
  
  # Only do this step if overview[[z]]$plots==TRUE
  
  if (overview[[z]]$plots==TRUE){
    
    vars_keep <- c(predictors) #"propensity_score",
    tt <- tcBEIDs_year[, c("tcBEID", "matched", "matching_group",
                           "potential_control",  "treatment_group", "stratum",
                           vars_keep), with=FALSE]
    
    # Adjusted: calculate variance ratio
    tt[,
       paste0(vars_keep,".var.adjusted") := lapply(.SD, function(x) var(x[treatment_group==TRUE & !is.na(matching_group)])/var(x[treatment_group==FALSE  & !is.na(matching_group)])),
       .SDcols=vars_keep]
    
    # Unadjusted: calculate variance ratio
    tt[,
       paste0(vars_keep,".var.unadjusted") := lapply(.SD, function(x) var(x[treatment_group==TRUE])/var(x[treatment_group==FALSE])),
       .SDcols=vars_keep]
    
    # standardize vars_keep by sd in treatment_group
    tt[, (vars_keep) := lapply(.SD, function(x) x/sd(x[treatment_group==TRUE])), .SDcols=vars_keep]
    
    # Adjusted: calculate mean difference between treated and matched controls within matching_group
    tt[,
       paste0(vars_keep,".adjusted") := lapply(.SD, function(x) mean(x[treatment_group==TRUE & !is.na(matching_group)])-mean(x[treatment_group==FALSE  & !is.na(matching_group)])),
       by=c("matching_group"), .SDcols=vars_keep]
    
    
    # Unadjusted: calculate mean difference between treated and control 
    tt[,
       paste0(vars_keep,".unadjusted") := lapply(.SD, function(x) x-mean(x[treatment_group==FALSE])), .SDcols=vars_keep]
    tt[treatment_group==FALSE, paste0(vars_keep,".unadjusted"):=NA_real_]
    
    # add number of matching_groups per stratum
    tt[, N := uniqueN(matching_group[!is.na(matching_group)]), by=stratum]
    
    # remove unnecessary cols
    tt[, c("tcBEID", "treatment_group", "matched", "potential_control", vars_keep) := NULL]
    
    # only keep one row per matching_group
    tt <- tt[, .SD[1],by=matching_group]
    
    # turn var ratios into data.table for ggplot2
    vv <- data.table(var=vars_keep, adjusted=t(tt[, lapply(.SD, function(x) x[1]), .SDcols=paste0(vars_keep, ".var.adjusted")]),
                     unadjusted=t(tt[, lapply(.SD, function(x) x[1]), .SDcols=paste0(vars_keep, ".var.unadjusted")]))
    
    
    # turn means into a data.table for ggplot2
    tt <- data.table(var=vars_keep, adjusted=t(tt[!is.na(matching_group), lapply(.SD, function(x) mean(x)), .SDcols=paste0(vars_keep, ".adjusted")]),
                     se.adjusted = t(tt[!is.na(matching_group), lapply(.SD, function(x) sd(x)/sqrt(length(x))), .SDcols=paste0(vars_keep, ".adjusted")]),
                     unadjusted=t(tt[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), .SDcols=paste0(vars_keep, ".unadjusted")]),
                     se.unadjusted=t(tt[, lapply(.SD, function(x) sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))), .SDcols=paste0(vars_keep, ".unadjusted")]))
    
    ###### create output table for paper #####
    # in the end, I want a table for the paper, so let's just extract the mean differences in tt and variance ratios in vv here
    output_table <- data.table(tt[, .(var=var, diff.unadjusted=unadjusted.V1, diff.adjusted=adjusted.V1)], 
                               vv[, .(var.unadjusted=unadjusted.V1, var.adjusted=adjusted.V1)])
    
    # create output table of standard errors
    output_table.se <- data.table(tt[, .(var=var, se.unadjusted=se.unadjusted.V1, se.adjusted=se.adjusted.V1)])
    

    # change tables for graph
    vv <- vv[, .(type=c("Adjusted Sample", "Unadjusted Sample"), value=c(adjusted.V1, unadjusted.V1)),by=var]
    tt <- tt[, .(type=c("Adjusted Sample", "Unadjusted Sample"), value=c(adjusted.V1, unadjusted.V1)),by=var]
    
    
    # adjust the labels & order (order according to label order)
    dict <- list(propensity_score = "Propensity Score",
                 mean_lrhwage = "Mean ln wage",
                 mean_lrhwage.change = "Mean ln wage 1-year growth rate",
                 mean_lrhwage.2change = "Mean ln wage 2-year growth rate",
                 log_wage ="Ln wage",
                 log_wage.change="Ln wage 1-year growth rate",
                 log_wage.2change="Ln wage 2-year growth rate",
                 log_employment="Ln employment",
                 log_employment.change = "Ln employment 1-year growth rate",
                 log_employment.2change = "Ln employment 2-year growth rate",
                 firm_FE = "Firm fixed effect",
                 firm_FE.lag1 = "Lag firm fixed effect",
                 firm_FE.change1 = "Firm fixed effect 1-year growth rate",
                 firm_FE.change2 =  "Lag firm fixed effect 1-year growth rate" ,
                 firm_FE.2change = "Firm fixed effect 2-year growth rate",
                 workerFE = "Mean worker fixed effect",
                 workerFE.lag1 = "Lag mean worker fixed effect",
                 workerFE.change1 =  "Mean worker fixed effect 1-year growth rate",
                 workerFE.change2 =  "Lag mean worker fixed effect fixed effect 1-year growth rate",
                 workerFE.2change = "Mean worker fixed effect 2-year growth rate",
                 mean_age_profile = "Mean wage-age profile",
                 mean_age_profile.change1 = "Mean wage-age profile 1-year growth rate",
                 mean_age_profile.2change = "Mean wage-age profile 2-year growth rate",
                 
                 workerFE_var = "Variance worker fixed effects",
                 log_firmage = "Ln firm age",
                 log_exports = "Ln exports")
    
    # output_table to latex
    output_table[, order:=0]
    output_table[var %in% names(dict), c("var", "order") := list(var=as.character(dict[which(names(dict)==var)]), order=which(names(dict)==var)) , by=var]
    setorder(output_table, order)
    setnames(output_table, 1:3, c("", "Unmatched", "Matched"))
    
    output_table[, .(before=mean(Unmatched), after=mean(Matched))]
    overview[[z]]$output_table <- copy(output_table)
    
    # need to edit this one in latex    
    print(xtable(output_table[, 1:3],
           caption = "Covariate balance before and after Propensity Score Matching",
           lab = "tab:PSM",
           digits=5), 
          file=paste0(map_output,"PSM/table_PSM_",z,".tex"), include.rownames=FALSE,
          caption.placement="top")
    
    
    # clearn up
    rm(output_table)
    
    ### output table standard errors to latex ---
    # create output table of standard errors
    output_table.se[, order:=0]
    output_table.se[var %in% names(dict), c("var", "order") := list(var=as.character(dict[which(names(dict)==var)]), order=which(names(dict)==var)) , by=var]
    setorder(output_table.se, order)
    setnames(output_table.se, 1:3, c("", "Unmatched", "Matched"))
    
    overview[[z]]$output_table.se <- copy(output_table.se)
    
    # need to edit this one in latex  and merge with table saved above
    print(xtable(output_table.se[, 1:3],
                 caption = "Covariate balance before and after Propensity Score Matching",
                 lab = "tab:PSM",
                 digits=5), 
          file=paste0(map_output,"PSM/table_PSM_se_",z,".tex"), include.rownames=FALSE,
          caption.placement="top")
    
    rm(output_table.se)
    
    # plots 
    tt[, order:=0]
    tt[var %in% names(dict), c("var", "order") := list(var=as.character(dict[which(names(dict)==var)]), order=which(names(dict)==var)) , by=var]
    setorder(tt, order)
    
    vv[, order:=0]
    vv[var %in% names(dict), c("var", "order") := list(var=as.character(dict[which(names(dict)==var)]), order=which(names(dict)==var)) , by=var]
    setorder(vv, order)
    
    # plot variane ratio and save it
    ggplot(data=vv, aes(x=reorder(var, -order), colour=type)) + geom_point(aes(y=value), size=2) + 
      geom_hline(yintercept=1) + 
      geom_hline(yintercept = 1/2, linetype="dashed") +
      geom_hline(yintercept = 2, linetype="dashed") +
      ylab("Variance ratio") + xlab("") +
      scale_y_continuous(breaks=seq(min(c(min(vv$value, 0.4))), max(c(max(vv$value,2))), 0.2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0, hjust=0, margin=margin(0,0,0,0)),
            legend.position = "bottom") + 
      coord_flip()
    ggsave(paste0(map_output,"PSM/plot_covariate_balance_var_ratio_",z,".pdf"), plot=last_plot())
    
    # plot mean deviation and save it
    ggplot(data=tt, aes(x=reorder(var, -order), colour=type)) + geom_point(aes(y=value), size=2) + 
      geom_hline(yintercept=0) + 
      geom_hline(yintercept = 0.1, linetype="dashed") +
      geom_hline(yintercept = -0.1, linetype="dashed") +
      ylab("Standardized Mean Difference") + xlab("") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle=0, vjust=0, hjust=0, margin=margin(0,0,0,0)),
            legend.position = "bottom") + 
      coord_flip()
    ggsave(paste0(map_output,"PSM/plot_covariate_balance_",z,".pdf"), plot=last_plot())
    rm(tt, vv)
  }
  
  # save dataset of matched and unmatched firms to use for DiD without PSM
  if (z==1) saveRDS(tcBEIDs_year, paste0(map_output_here,"tcBEIDs_ana_matched_unmatched_z_",z,".rds"), compress=TRUE)
  
  
  #### Prepare for merge ----
  # subset to matched data set
  tcBEIDs_year <- tcBEIDs_year[!is.na(matching_group),]
  
  # reassing treatment and control group
  tcBEIDs_year[, treatment_group2 := treatment_group]
  tcBEIDs_year[, control_group := !treatment_group2]
  tcBEIDs_year[control_group==TRUE, placebo_takeover_year := year-overview[[z]]$time_takeover]
  
  # extract info for merge
  tcBEIDs_year <- tcBEIDs_year[, c("tcBEID", "matching_group", "treatment_group2", "control_group", 
                                   "placebo_takeover_year", "NA_before", "propensity_score",
                                   "stratum")]
  
  #### save overview ----
  saveRDS(overview, paste0(map_output_here,"PSM_overview.rds"), compress=TRUE)
  
  ### also save small tcBEIDs_year table ----
  # tcBEIDs_year[, mean_lrhwage_stayer1 := NULL]
  saveRDS(tcBEIDs_year, paste0(map_output_here,"tcBEIDs_ana_PSM_z_",z,".rds"), compress=TRUE)
  gc()
}
rm(z)
rm(tcBEIDs_year, overview, dict)
rm(tcBEIDs_year_ana)
gc()