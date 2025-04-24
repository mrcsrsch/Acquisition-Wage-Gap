##################################################################################
### Analses on mechanisms ######
##################################################################################

#### required packages ----
if (!require("fixest")) install.packages("fixest"); library("fixest")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")


#### Output table styles & dict ----
style_tex <- style.tex(main="aer", 
                       model.title="",
                       #var.title="Years since takeover",
                       line.top = "\\toprule", line.bottom = "\\bottomrule",
                       fixef.title="\\midrule Fixed-effects",
                       stats.title="\\midrule", 
                       notes.intro="\\medskip \\raggedright \\footnotesize \\emph{Notes:}",
                       fixef.suffix = "",
                       fixef_sizes.prefix = "\\# ",
                       fixef.where="var") 

mydict <- c(            # general
  firm_FE="Firm fe", 
  mean_lrhwage = "Mean ln wage",
  workerFE="Mean worker fe",
  age_profile = "Mean wage-age pr",
  tcBEID = "Firm ID",
  matching_group.year = "Pair-year",
  log_wage = "Ln mean wage",
  workerFE_var = "Var worker fe",
  log_employment="Ln employment",
  percentage_workerFE_q1 = "Percentage first quantile",
  percentage_workerFE_q34 = "Percentage third/fourth quantile",
  # Stayers
  mean_lrhwage_stayer1 = "Mean ln wage",
  age_profile_stayer = "Mean wage-age pr",
  resid_stayer = "Mean residual",
  
  # Inmovers
  mean_lrhwage_in = "Mean ln wage",
  workerFE_in = "Mean worker fe",
  age_profile_in = "Mean wage-age pr",
  resid_in = "Mean residual",
  # Outmovers
  mean_lrhwage_out = "Mean ln wage",
  workerFE_out = "Mean worker fe",
  #firm_FE = "Firm Fixed Effect",
  age_profile_out = "Mean wage-age pr",
  resid_out = "Mean residual",
  
  # ages
  mean_age = "All",
  mean_age_in = "New hires",
  mean_age_out = "Leavers",
  
  `time_takeover::-4:treatment_group2` = "s=-4", #"$\\delta_{-4}$",
  `time_takeover::-3:treatment_group2` = "s=-3", #"$\\delta_{-3}$",
  `time_takeover::-2:treatment_group2` ="s=-2", #"$\\delta_{-2}$",
  `time_takeover::-1:treatment_group2` = "s=-1", #"$\\delta_{-1}$",
  `time_takeover::0:treatment_group2` = "s=0", #"$\\delta_{0}$",
  `time_takeover::1:treatment_group2` = "s=1", #"$\\delta_{1}$",
  `time_takeover::2:treatment_group2` = "s=2", #"$\\delta_{2}$",
  `time_takeover::3:treatment_group2` = "s=3", 
  `time_to_treatment::-3` = "s=-3",
  `time_to_treatment::-2` = "s=-2",
  `time_to_treatment::0` = "s=0",
  `time_to_treatment::1` = "s=1",
  `time_to_treatment::2` = "s=2",
  `time_to_treatment::3` = "s=3", 
  `post::TRUE:treatment_group2` = "Post-Acquisition",
  `matching_group^post` = "Pair-post",
  `matching_group^year` = "Pair-year") #"$\\delta_{3}$")



#### functions #####
# function to calculate the FIRST mode
first_mode <- function(vec, remove.NA = TRUE, char_type = TRUE){
  if (remove.NA == TRUE) vec <- vec[!is.na(vec)]
  if (length(vec) == 0) {if (char_type==TRUE) return(NA_character_) else return(NA)} # this is to ensure that NA is return if vec is empty
  vec_uniq <- unique(vec) 
  return(vec_uniq[which.max(tabulate(match(vec,vec_uniq)))])
}

#### load data ----
# Data set for analysis: PSM matched dataset 
tcBEIDs_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))

# load full employer employee network
RINPs_year <- readRDS(paste0(map_data,"step9/RINPs_year_numeric.rds"))

##################################################################################
##################################################################################

#################################################
#### prepare data (general) #####
# subset data to event time
tcBEIDs_ana <- tcBEIDs_ana[(time_takeover>=-3 & time_takeover<=3), ]
# add post var
tcBEIDs_ana[, post := time_takeover>=0]
# add age profile tcBEIDs_ana
tt <- RINPs_year[tcBEID_year %in% tcBEIDs_ana[, tcBEID_year], .(age.profile = mean(age.profile)), by = tcBEID_year]
tcBEIDs_ana <- merge(tcBEIDs_ana, tt, by="tcBEID_year")
rm(tt)


#################################################
#### 1. Training & 2. Rent appropriation #####
##### prepare #####
# find hires and separations
RINPs <- RINPs_year[tcBEID_year %in% tcBEIDs_ana[, unique(tcBEID_year)], unique(RINP)]
setorderv(RINPs_year, c("RINP", "year"))
RINPs_year[RINP %in% RINPs, c("tcBEID.lag", "tcBEID.lead") := shift(tcBEID, n=c(-1,1), type="lead"), by=RINP]
RINPs_year[RINP %in% RINPs, exit := (tcBEID.lead != tcBEID) | (year<2018 & is.na(tcBEID.lead))]
RINPs_year[RINP %in% RINPs, entry := (tcBEID.lag != tcBEID) | (year>2006 & is.na(tcBEID.lag))]
RINPs_year[, c("tcBEID.lag", "tcBEID.lead") := NULL]

##### 1. Rent appropriation ##### 
###### load bestuurders data ##### 
best <- list()
i <- 0
for (year in 2010:2018){
  # message 
  cat(paste(Sys.time(), "reading", year), "\n")
  i <- i + 1
  best[[i]] <- fread(file=paste0(map_data2, "FCV", year, "_Kenmerken_blogCBKV1.csv"))
  best[[i]][, "year" := year]
}
best <- rbindlist(best)

# create numeric RINP
best[, RINP := paste0(CBKSoortNR, Rinpersoon)]
best <- best[, c("RINP", "year")]
best[, ISCO08_first := 1] # set ISCO08_first to group "Managers"
# load RINP translation table
trans_RINP <- readRDS(paste0(map_data,"step9/trans_RINP.rds"))
best <- merge(best, trans_RINP, by="RINP")
best[, RINP := NULL]
setnames(best, "RINP_n", "RINP")
rm(trans_RINP)

###### load data EBB data - subsample selection #####
# load RINP translation table
trans_RINP <- readRDS(paste0(map_data,"step9/trans_RINP.rds"))


# load EBBs 
t <- 0
colClass <- c("character", "character", "NULL", "character", "integer")
EBB <- list()
for (current_year in 2006:2018){
  # message
  cat(paste(Sys.time(), "Starting with year:", current_year), "\n")
  t <- t+1
  
  # load EBB
  path <- paste0(map_data2,"EBB/EBBBEROEP/", current_year, "/DATA/EBB", current_year, "1299ANAV1.csv")
  EBB[[t]] <- fread(path, sep=";",colClasses = colClass)
}
rm(t, path, colClass, current_year)
EBB <- rbindlist(EBB)
setnames(EBB, c("EBBTW1ISCO2008V", "jaar"), c("ISCO08", "year"))
# create RINP
EBB[, RINP := paste0(RINPERSOONS, RINPERSOON)]
EBB[, c("RINPERSOONS", "RINPERSOON") := NULL]

# add numeric RINP
EBB <- merge(EBB, trans_RINP, by="RINP")
EBB[, RINP := NULL]
setnames(EBB, "RINP_n", "RINP")
rm(trans_RINP)

# only keep unique combinations in EBB
EBB <- unique(EBB)

# some workers have multiple observed ISCO08 Codes per year
## set ISCO08_first to NA for ISCO08 starting with 10 and ISCO08 == 0000  (armed forces and category is unknown)
EBB[substr(ISCO08, 1,2)=="10" | ISCO08=="0000", ISCO08 := NA]
EBB[, ISCO08_first := as.numeric(substr(ISCO08, 1,1))]
EBB[, ISCO08 := NULL]

###### combine data #####
EBB <- rbindlist(list(EBB, best), use.names=TRUE)
rm(best)
EBB <- unique(EBB)


###### identify broad categories of occupations and delete doubles #####
# we only differentiate managers and non-managers  now
EBB[, occ_group := fifelse(ISCO08_first==1, 1, 2)]
EBB[, .N]
EBB <- unique(EBB[, c("RINP", "year", "occ_group")])
EBB[, .N]
## some workers have multiple observation per year: assign highest occupation group
EBB[, occ_group := min(occ_group), by=c("RINP", "year")]
EBB <- unique(EBB[, c("RINP", "year", "occ_group")])
EBB[, .N]

###### create RINPs_ana and prepare data #####
# create RINPs_ana with worker occupations (workers in matched sample)
# i.e. subsample RINPs_year to workers in matched sample 
# and for whom occupation is known, at least at some point
RINPs_ana <- merge(RINPs_year, tcBEIDs_ana[, c("tcBEID_year", "treatment_group2", "time_takeover", "matching_group")], by=c("tcBEID_year"))
RINPs_ana[, .N]
RINPs_ana <- merge(RINPs_ana, EBB, by=c("RINP", "year"), all.x=TRUE)
RINPs_ana[, .N]
rm(EBB)

# NAs are occupation group 2
RINPs_ana[is.na(occ_group), occ_group := 2]
# assign lowest occupation group per match
RINPs_ana[, occ_group := min(occ_group), by=c("RINP", "tcBEID")]

# only keep firms with observed managers and non-managers
RINPs_ana[, .(.N, uniqueN(tcBEID))]
RINPs_ana[, keep := any(occ_group==1) & any(occ_group==2), by=tcBEID_year]
RINPs_ana <- RINPs_ana[keep==TRUE, !c("keep")]
RINPs_ana[, .(.N, uniqueN(tcBEID))]

###### aggregate to tcBEID_year-occupation-group level ##### 
wages_occupations <- RINPs_ana[, lapply(.SD, mean), by=c("tcBEID_year", "occ_group"), 
                               .SDcols=c("lrhwage", "firm_FE", "worker_FE", "age.profile", "resid.AKM")]
# add number of workers per group
 wages_occupations <- merge(wages_occupations, RINPs_ana[, .(obs=.N, exits=sum(exit), entries=sum(entry)), by=c("tcBEID_year", "occ_group")],
                            by =c("tcBEID_year", "occ_group") )

# and add vars from tcBEIDs_ana
wages_occupations <- merge(wages_occupations, 
                           tcBEIDs_ana[, c("tcBEID_year", "tcBEID", "year", "workers", "treatment_group2", "post", "time_takeover", "matching_group")],
                           by = "tcBEID_year")

wages_occupations[, c("exit_rate", "entry_rate") := .(exits/obs, entries/obs)]

###### Run DiD on wages per group (event study) ##### 
dep.vars <- c("lrhwage", "firm_FE",  "worker_FE",  "age.profile",   "resid.AKM")
groups <- wages_occupations[, unique(occ_group)]


occupation_estimates <- vector(mode="list", length=length(groups))
exit_rates <- vector(mode="list", length=length(groups))
entry_rates <- vector(mode="list", length=length(groups))

walds <- vector(mode="list", length=length(groups))

for (g in groups){
  # set subset to occupation group
  wages_occupations[, subset := (occ_group == g)]
  
  # sample selection: observed at s=-1, only s with both in pair
  wages_occupations[subset==TRUE, subset := sum(!is.na(eval(as.symbol(dep.vars[1]))))>1, by=c("time_takeover", "matching_group")]
  # wages_occupations[subset==TRUE, subset := any(time_takeover==-1), by=c("matching_group")]
  
  for (i in 1:length(dep.vars)){
    # message
    cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
    
    # calculate estimate 
    occupation_estimates[[g]][[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                         " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                       data=wages_occupations,
                                       subset = (wages_occupations$subset),
                                       # ONE WAY CLUSTERING
                                       cluster="tcBEID",
                                       #weights = wages_occupations$obs,
                                       ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
    

    # wald tests
    walds[[g]][[i]] <- wald(occupation_estimates[[g]][[i]], keep="(ti.*er)::-")
    
  }
  # calculate estimate
  exit_rates[[g]] <- feols(as.formula(paste0("exit_rate", 
                                                  " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                data=wages_occupations,
                                subset = (wages_occupations$subset),
                                # ONE WAY CLUSTERING
                                cluster="tcBEID",
                                #weights = wages_occupations$obs,
                                ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  # calculate estimate
  entry_rates[[g]] <- feols(as.formula(paste0("entry_rate", 
                                             " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                           data=wages_occupations,
                           subset = (wages_occupations$subset),
                           # ONE WAY CLUSTERING
                           cluster="tcBEID",
                           #weights = wages_occupations$obs,
                           ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  # remove subset
  wages_occupations[, subset := NULL]
  
}


etable(occupation_estimates[[1]])
etable(occupation_estimates[[2]])

etable(exit_rates)
etable(entry_rates)
###### Run DiD on wages per group (simple) ##### 
dep.vars <- c("lrhwage", "firm_FE",  "worker_FE",  "age.profile",   "resid.AKM")
groups <- wages_occupations[, unique(occ_group)]


occupation_estimates_simple <- vector(mode="list", length=length(groups)+1)
exit_rates_simple <- vector(mode="list", length=length(groups))
entry_rates_simple <- vector(mode="list", length=length(groups))

for (g in groups){
  # # set subset to occupation group
wages_occupations[, subset := (occ_group == g)]
  
  # select groups for simple DiD
  #wages_occupations[subset==TRUE, subset := any(!is.na(lrhwage[treatment_group2 & post])) & any(!is.na(lrhwage[!treatment_group2 & post])) &
  #              any(!is.na(lrhwage[treatment_group2 & !post])) & any(!is.na(lrhwage[!treatment_group2 & !post])), by=c("matching_group")]
wages_occupations[subset==TRUE, subset := any(!is.na(lrhwage[treatment_group2])) & any(!is.na(lrhwage[!treatment_group2])), by=c("matching_group", "post")]
  
  for (i in 1:length(dep.vars)){
    # message
    cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
    
    # calculate estimate (simple DiD)
    occupation_estimates_simple[[g]][[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                                     " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                                   data=wages_occupations,
                                            subset = (wages_occupations$subset),
                                            # ONE WAY CLUSTERING
                                            cluster="tcBEID",
                                            #weights = wages_occupations$obs,
                                            ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
    

  }

  # calculate estimate
  exit_rates_simple[[g]] <- feols(as.formula(paste0("exit_rate",
                                             " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^year")),
                           data=wages_occupations,
                           subset = (wages_occupations$subset),
                           # ONE WAY CLUSTERING
                           cluster="tcBEID",
                           #weights = wages_occupations$obs,
                           ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  # calculate estimate
  entry_rates_simple[[g]] <- feols(as.formula(paste0("entry_rate",
                                              " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^year")),
                            data=wages_occupations,
                            subset = (wages_occupations$subset),
                            # ONE WAY CLUSTERING
                            cluster="tcBEID",
                            #weights = wages_occupations$obs,
                            ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  # remove subset
  wages_occupations[, subset := NULL]
}


etable(occupation_estimates_simple[[1]])
etable(occupation_estimates_simple[[2]])

etable(exit_rates_simple)
etable(entry_rates_simple)

###### Run DiD on wages per group (interaction) ##### 
wages_occupations[, subset := any(!is.na(lrhwage[treatment_group2])) & any(!is.na(lrhwage[!treatment_group2])), by=c("matching_group", "post")]
wages_occupations[, manager := occ_group==1]

for (i in 1:length(dep.vars)){
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  occupation_estimates_simple[[3]][[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                                   " ~ i(factor_var = post, var=treatment_group2, ref=0)+ i(factor_var = post, var=treatment_group2, ref=0):manager | tcBEID^manager + matching_group^post^manager")), 
                                                 data=wages_occupations,
                                                 subset = (wages_occupations$subset),
                                                 # ONE WAY CLUSTERING
                                                 cluster="tcBEID",
                                                 #weights = wages_occupations$obs,
                                                 ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  
}

wages_occupations[, subset := NULL]

etable(occupation_estimates_simple[[3]])

###### Save output tables ######
for (g in groups){
  etable(occupation_estimates[[g]], 
         title=paste0("Difference-in-Differences: Occupation group ", g),
         label=paste0("tab:occ_", g),
         float=TRUE,
         fixef_sizes = TRUE,
         depvar=TRUE,
         digits="r4",
         digits.stats="r4",
         cluster="tcBEID", #  
         dict = mydict,
         fitstat=c("n", "r2"),
         signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
         notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                   "Clustered standard errors (Firm ID) in parantheses.",
                   "Ln is the the natural logarithm.",
                   "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
         extralines = list("\\midrule Pre-trends \\ P-value"=unlist(lapply(paste0("round(walds[[",g,"]][[",
                                                                                                   1:length(walds[[g]]),
                                                                                                   "]]$p, 4)"), 
                                                                                            function(x) eval(parse(text=x))))),
         se.below = TRUE,
         tex=TRUE,
         style.tex = style_tex,
         replace=TRUE,
         file=paste0(map_output_here, "REV1_1_occ_", g, ".tex"))
}

for (g in groups){
  etable(occupation_estimates_simple[[g]], 
         title=paste0("Difference-in-Differences: Occupation group ", g),
         label=paste0("tab:occ_", g),
         float=TRUE,
         fixef_sizes = TRUE,
         depvar=TRUE,
         digits="r4",
         digits.stats="r4",
         cluster="tcBEID", #  
         dict = mydict,
         fitstat=c("n", "r2"),
         signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
         notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                   "Clustered standard errors (Firm ID) in parantheses.",
                   "Ln is the the natural logarithm.",
                   "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
         se.below = TRUE,
         tex=TRUE,
         style.tex = style_tex,
         replace=TRUE,
         file=paste0(map_output_here, "REV1_1_occ_simple_", g, ".tex"))
}

# print interaction results with degrees of freedom
etable(occupation_estimates_simple[[3]], 
       title=paste0("Difference-in-Differences: Occupation group; INTERACTION"),
       label=paste0("tab:occ_interact"),
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2", "g"), # also return t-test degrees of freedom
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_occ_simple_interact",".tex"))

rm(walds, occupation_estimates, occupation_estimates_simple)

#################################################
##### 2. Worker training #####

###### prepare data / subsample selection from RINPs_year #### 
# only keep movers
RINPs_year <- RINPs_year[RINP %in% RINPs_year[entry==TRUE, unique(RINP)],]

# get tcBEID_year.lag
RINPs_year[, tcBEID_year.lag := shift(tcBEID_year, n=1, type="lag"), by=RINP]

# identify relevant combinations of RINPs and tcBEIDs (i.e. those leaving firms in event time of matched sample)
RINPs_year[entry==TRUE, mover_select := tcBEID_year.lag %in% tcBEIDs_ana[, unique(tcBEID_year)]]
RINPs_year[, mover_select := any(mover_select, na.rm = T), by=c("RINP", "tcBEID")]
RINPs_year <- RINPs_year[mover_select==TRUE, !c("mover_select")]

# remove NAs
RINPs_year <- RINPs_year[!is.na(entry), ]

# expand mover and tcBEID_year.lag over remaining spell in new firm
RINPs_year[, mover_new := entry]
RINPs_year[, exit.save := exit]
RINPs_year[is.na(exit), exit := TRUE]

RINPs_year[, c("mover_new", "tcBEID_year.lag") := lapply(.SD, function(x){
  # check if any mover
    #cat(paste(RINP[1], " ", tcBEID[1], " - "))
    # take first mover position and expand x
  # identify start and end points
  starts <- which(entry)
  ends <- which(exit)
  years <- sapply(starts, function(x) x:min(ends[ends>=x]))
  starts <- unlist(sapply(years, function(x) rep(x[1], length(x))))
  years <- unlist(years)

  x[years] <- x[starts]

  return(x)
}), by=c("RINP"), .SDcols =  c("mover_new", "tcBEID_year.lag")]

# update mover
RINPs_year[, entry := mover_new]
RINPs_year[, mover_new := NULL]

# subset to selected entry observations
RINPs_year <- RINPs_year[entry==TRUE, !c("entry")]

###### Aggregate to firm level ##### 
mover_vars <- c("lrhwage",  "firm_FE", "worker_FE", "age.profile", "resid.AKM") # "year_fe")
tt <- RINPs_year[, sapply(.SD, function(x) mean(x), simplify = FALSE, USE.NAMES = TRUE), 
                 by=c("tcBEID_year.lag"), 
                 .SDcols=mover_vars]
tt <- merge(tt, RINPs_year[, .(movers = .N), by="tcBEID_year.lag"], by="tcBEID_year.lag")
setnames(tt, c("tcBEID_year", paste0("mover_", mover_vars), "mover_movers"))

# add to analysis table
mover_vars <- paste0("mover_", mover_vars)
tcBEIDs_ana[, c(mover_vars, "mover_movers") := NULL]
tcBEIDs_ana <- merge(tcBEIDs_ana, tt, by="tcBEID_year", all.x=TRUE)

######  Event Study: Run DiD on movers' wages in new firm ##### 
dep.vars <- c(mover_vars)

mover_estimates_event <- vector(mode="list", length=length(dep.vars))
walds <- vector(mode="list", length=length(dep.vars))


tcBEIDs_ana[, sample := TRUE]
for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  #rows_delete <- tcBEIDs_ana[rows, .I[any(is.na(eval(as.symbol(dep.vars[i]))[which(time_takeover==-1)])) & (is.logical(treatment_group2))], by=matching_group][, V1]
  #tcBEIDs_ana[sample==TRUE, sample := !any(is.na(eval(as.symbol(dep.vars[i]))[time_takeover==-1])), by=matching_group]
  
  # whereever dep.var is NA in for any obs in matching group at t, remove t for matching group from analysis
  #tcBEIDs_ana[sample==TRUE, sample := !any(is.na(eval(as.symbol(dep.vars[i])))), by=c("time_takeover", "matching_group")]
  tcBEIDs_ana[sample==TRUE, sample := sum(!is.na(eval(as.symbol(dep.vars[i]))))>1, by=c("time_takeover", "matching_group")]
  #tcBEIDs_ana[sample==TRUE, sample := any(time_takeover==-1), by=c("matching_group")]
  
  
  # calculate estimate (simple DiD)
  mover_estimates_event[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                        " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                      data=tcBEIDs_ana,
                                      subset = tcBEIDs_ana$sample,
                                      # ONE WAY CLUSTERING
                                      cluster="tcBEID",
                                      ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  # wald tests
  walds[[i]] <- wald(mover_estimates_event[[i]], keep="(ti.*er)::-")
  

}
# calculate firm with observation per time_takeover
tcBEIDs_ana[treatment_group2==T, sum(sample), by=time_takeover]
tcBEIDs_ana[, sum(mover_movers[sample]), by=time_takeover]


# how many treated firms are in sample in t==3 and t==-1? 

etable(mover_estimates_event)
tcBEIDs_ana[sample==T, .(t3 = any(time_takeover==3), both = any(time_takeover==3) & any(time_takeover==-1)), by=tcBEID][, sum(both)/sum(t3)*100]
tcBEIDs_ana[sample==T, .(t1 = any(time_takeover==-1), both = any(time_takeover==3) & any(time_takeover==-1)), by=tcBEID][, sum(both)/sum(t1)*100]

####### Save coefplot of residuals and identiying firms #####
pdf(file=paste0(map_output_here, "REV1_1_resid_mover.pdf"), width=16, height=9)
iplot(mover_estimates_event[[5]], main = "Event study estimates (AKM residual at new employer)", xlab="Years since acquisition")
dev.off()


ggplot(data=tcBEIDs_ana[, sum(sample), by=time_takeover], 
       aes(x=time_takeover, y=V1)) + geom_point() +
  xlab("Years since acquisition") + ylab("# of firms") +
  scale_x_continuous(breaks=-3:3)  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(0,0,0,0)),
        legend.position = "bottom", legend.title = element_blank(),
        legend.key = element_blank()) 
ggsave(paste0(map_output_here, "plot_REV1_1_mover_firms.pdf"), plot=last_plot(), dpi=300)



# reset sample
tcBEIDs_ana[, sample := NULL]

####### Collapsed DiD #####
# remove matching_groups with no pre-post combinations
tcBEIDs_ana[, sample := any(!is.na(mover_lrhwage[treatment_group2 & post])) & any(!is.na(mover_lrhwage[!treatment_group2 & post])) &
              any(!is.na(mover_lrhwage[treatment_group2 & !post])) & any(!is.na(mover_lrhwage[!treatment_group2 & !post])), by=c("matching_group")]

mover_estimates <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  mover_estimates[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                  " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                data=tcBEIDs_ana,
                                subset = tcBEIDs_ana$sample,
                                # ONE WAY CLUSTERING
                                cluster="tcBEID",
                                ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}
tcBEIDs_ana[, sample := NULL]

etable(mover_estimates)


####### Save output tables #####
etable(mover_estimates, 
       title="Difference-in-Differences: Mover wage decomposition (collapsed)",
       label="tab:mover_wages",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_mover_wages.tex"))


etable(mover_estimates_event, 
       title="Difference-in-Differences: Mover wage decomposition",
       label="tab:mover_wages",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       extralines = list("{title:\\midrule Pre-trends; where:stats}P-value"=unlist(lapply(paste0("round(walds[[",
                                                                                                1:length(walds),
                                                                                                "]]$p, 4)"), 
                                                                                         function(x) eval(parse(text=x))))),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_mover_wages_event.tex"))


rm(mover_estimates_event, mover_estimates, walds)


#################################################
#### Firm productivity / scale of operations ----
##### load dataset with financial data  ---
tcBEIDs_full <- readRDS(paste0(map_data,"step9/tcBEIDs_year_numeric.rds"))
###### prepare data / subsample selection ####
financial_vars <- c("sales_ps", "toegevoedgewaarde_factorkosten",  "productiewaarde",
                    "export_value", "export_countries", "import_value", "import_countries", "employees_ps")
# financial variables
# Add productivity measures to tcBEIDs_ana for firms in the matched sample
tcBEIDs <- tcBEIDs_ana[, unique(tcBEID_year)]
prods <- tcBEIDs_full[tcBEID_year %in% tcBEIDs, paste0(c("tcBEID_year", financial_vars)), with=FALSE]

# remove vars from tcBEIDs_ana if they exist and add new ones
tcBEIDs_ana[, (financial_vars) := NULL]
tcBEIDs_ana <- merge(tcBEIDs_ana, prods, by=c("tcBEID_year"), all.x=TRUE)

# clean up
rm(tcBEIDs, prods)

###### other manipulations ##### 
# when <= 0, set sales_ps and toegevoedgewaarde_factorkosten to is.na()
# for value added: 131 obs in 93 firms
tcBEIDs_ana[,  c("sales_ps", "toegevoedgewaarde_factorkosten", "productiewaarde") := lapply(.SD, function(x) {x[x<=0] <- NA; x}), .SDcols= c("sales_ps", "toegevoedgewaarde_factorkosten", "productiewaarde")]


###### summary stats on missing values #####
# Missings in general
tcBEIDs_ana[, .(N_sales = sum(is.na(sales_ps)), firms_sales = uniqueN(tcBEID[is.na(sales_ps)]),
                N_va = sum(is.na(toegevoedgewaarde_factorkosten)), firms_va = uniqueN(tcBEID[is.na(toegevoedgewaarde_factorkosten)]))]

# percentage of observations missing per firm 
tcBEIDs_ana[, sum(is.na(sales_ps))/.N, by=tcBEID][, quantile(V1)]
# 

tcBEIDs_ana[, sum(is.na(toegevoedgewaarde_factorkosten))/.N, by=tcBEID][, quantile(V1)]


###### Run DiD regressions ####### 

####### employment, sales, value added ####### 
dep.vars <- c("workers", "sales_ps", "toegevoedgewaarde_factorkosten", "productiewaarde")

prod_estimates <- vector(mode="list", length=length(dep.vars))

# remove sample var if it exists
tcBEIDs_ana[, sample := NULL]
# set sample to rows where all dep.vars are observed
tcBEIDs_ana[, sample := !is.na(sales_ps) & !is.na(toegevoedgewaarde_factorkosten) & !is.na(productiewaarde) & toegevoedgewaarde_factorkosten>0 & productiewaarde>0]
# and where they are observed pre- and post per matching group
tcBEIDs_ana[sample==TRUE, sample := any(!is.na(get(dep.vars[2])[treatment_group2 & post])) & any(!is.na(get(dep.vars[2])[!treatment_group2 & post])) &
              any(!is.na(get(dep.vars[2])[treatment_group2 & !post])) & any(!is.na(get(dep.vars[2])[!treatment_group2 & !post])), by=c("matching_group")]

for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  prod_estimates[[i]] <- feols(as.formula(paste0("log(", dep.vars[i], ")", 
                                                 #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^post")), 
                                                 " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                               data=tcBEIDs_ana,
                               subset=tcBEIDs_ana$sample, 
                               # ONE WAY CLUSTERING
                               cluster="tcBEID",
                               ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(prod_estimates)


####### Wage decomposition ####### 
dep.vars <- c("mean_lrhwage", "firm_FE", "workerFE", "age.profile")


prod_wage_decomp <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # remove sample var if it exists
  #tcBEIDs_ana[, sample := NULL]
  
  # remove matching_groups with no pre-post combinations
  # tcBEIDs_ana[, sample := any(!is.na(get(dep.vars[i])[treatment_group2 & post])) & any(!is.na(get(dep.vars[i])[!treatment_group2 & post])) &
  #               any(!is.na(get(dep.vars[i])[treatment_group2 & !post])) & any(!is.na(get(dep.vars[i])[!treatment_group2 & !post])), by=c("matching_group")]
  # 
  #tcBEIDs_ana[, sample := TRUE]
  
  # calculate estimate (simple DiD)
  prod_wage_decomp[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                 #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^post")), 
                                                 " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                               data=tcBEIDs_ana,
                               subset=tcBEIDs_ana$sample, 
                               # ONE WAY CLUSTERING
                               cluster="tcBEID",
                               ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(prod_wage_decomp)



####### sales, value added per worker ####### 
dep.vars <- c("sales_ps", "toegevoedgewaarde_factorkosten", "productiewaarde")

prod_estimates_worker <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  prod_estimates_worker[[i]] <- feols(as.formula(paste0("log(", dep.vars[i], "/workers)", 
                                                 #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^post")), 
                                                 " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                               data=tcBEIDs_ana,
                               subset=tcBEIDs_ana$sample, 
                               # ONE WAY CLUSTERING
                               cluster="tcBEID",
                               ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}
# keep sample division for exports 

etable(prod_estimates_worker)

####### Exporting ####### 
######## Productivity sample ######
######### Poisson DiDs ########
dep.vars <- c("export_value", "export_countries")

exporting_estimates_sample <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  exporting_estimates_sample[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                            #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                                            " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                          data=tcBEIDs_ana,
                                          subset=tcBEIDs_ana$sample, 
                                          #     family = "poisson",
                                          # ONE WAY CLUSTERING
                                          cluster="tcBEID",
                                          fixef.rm="none", # don't remove matching groups with only 0s
                                          ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(exporting_estimates_sample)

######### Linear probability model ########
exporting_estimates_sample[[i+1]] <- feols(as.formula(paste0("export_value>0",
                                                           " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                         data=tcBEIDs_ana, 
                                         subset=tcBEIDs_ana$sample, 
                                         # ONE WAY CLUSTERING
                                         cluster="tcBEID",
                                         ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))

etable(exporting_estimates_sample)



######## Full sample ######
######### Poisson DiDs ########
dep.vars <- c("export_value", "export_countries")

exporting_estimates_full <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  exporting_estimates_full[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                 #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                                 " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                               data=tcBEIDs_ana,
                          #     family = "poisson",
                               # ONE WAY CLUSTERING
                               cluster="tcBEID",
                               fixef.rm="none", # don't remove matching groups with only 0s
                               ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(exporting_estimates_full)

######### Linear probability model ########
exporting_estimates_full[[i+1]] <- feols(as.formula(paste0("export_value>0",
                                               " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                             data=tcBEIDs_ana, 
                             # ONE WAY CLUSTERING
                             cluster="tcBEID",
                             ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))

etable(exporting_estimates_full)



######### Event DiD (pre-trends) #######
########## Poisson DiDs ########
dep.vars <- c("export_value", "export_countries")

exporting_estimates_event <- vector(mode="list", length=length(dep.vars))
exporting_walds <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  exporting_estimates_event[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                             " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                           data=tcBEIDs_ana,
                                           #     family = "poisson",
                                           # ONE WAY CLUSTERING
                                           cluster="tcBEID",
                                           fixef.rm="none", # don't remove matching groups with only 0s
                                           ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  # wald tests
  exporting_walds[[i]] <- wald(exporting_estimates_event[[i]], keep="(ti.*er)::-")
  
}

########## Linear probability model ########
exporting_estimates_event[[i+1]] <- feols(as.formula(paste0("export_value>0",
                                                            " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                          data=tcBEIDs_ana, 
                                          # ONE WAY CLUSTERING
                                          cluster="tcBEID",
                                          ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
# wald tests
exporting_walds[[i+1]] <- wald(exporting_estimates_event[[i+1]], keep="(ti.*er)::-")

etable(exporting_estimates_event)


####### Importing ####### 
######## Productivity sample ######
######### Poisson DiDs ########
dep.vars <- c("import_value", "import_countries")

importing_estimates_sample <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  importing_estimates_sample[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                            #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                                            " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                          data=tcBEIDs_ana,
                                          subset=tcBEIDs_ana$sample,
                                          #     family = "poisson",
                                          # ONE WAY CLUSTERING
                                          cluster="tcBEID",
                                          fixef.rm="none", # don't remove matching groups with only 0s
                                          ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(importing_estimates_sample)

######### Linear probability model ########
importing_estimates_sample[[i+1]] <- feols(as.formula(paste0("import_value>0",
                                                           " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                         data=tcBEIDs_ana, 
                                         subset=tcBEIDs_ana$sample,
                                         # ONE WAY CLUSTERING
                                         cluster="tcBEID",
                                         ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))

etable(importing_estimates_sample)


######## Full sample ######
######### Simple DiD #######
########## Poisson DiDs ########
dep.vars <- c("import_value", "import_countries")

importing_estimates_full <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  importing_estimates_full[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                       #" ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                                       " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                     data=tcBEIDs_ana,
                                     #     family = "poisson",
                                     # ONE WAY CLUSTERING
                                     cluster="tcBEID",
                                     fixef.rm="none", # don't remove matching groups with only 0s
                                     ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}

etable(importing_estimates_full)

########## Linear probability model ########
importing_estimates_full[[i+1]] <- feols(as.formula(paste0("import_value>0",
                                                      " ~ i(factor_var = post, var=treatment_group2, ref=0) | tcBEID + matching_group^post")), 
                                    data=tcBEIDs_ana, 
                                    # ONE WAY CLUSTERING
                                    cluster="tcBEID",
                                    ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))

etable(importing_estimates_full)


######### Event DiD (pre-trends) #######
########## Poisson DiDs ########
dep.vars <- c("import_value", "import_countries")

importing_estimates_event <- vector(mode="list", length=length(dep.vars))
importing_walds <- vector(mode="list", length=length(dep.vars))


for (i in 1:length(dep.vars)){
  
  # message
  cat(paste0(Sys.time(), " estimating ", dep.vars[i]), "\n")
  
  # calculate estimate (simple DiD)
  importing_estimates_event[[i]] <- fepois(as.formula(paste0(dep.vars[i], 
                                                            " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                          data=tcBEIDs_ana,
                                          #     family = "poisson",
                                          # ONE WAY CLUSTERING
                                          cluster="tcBEID",
                                          fixef.rm="none", # don't remove matching groups with only 0s
                                          ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  # wald tests
  importing_walds[[i]] <- wald(importing_estimates_event[[i]], keep="(ti.*er)::-")
  
}

etable(importing_estimates_event)

########## Linear probability model ########
importing_estimates_event[[i+1]] <- feols(as.formula(paste0("import_value>0",
                                                            " ~ i(factor_var = time_takeover, var=treatment_group2, ref=-1) | tcBEID + matching_group^year")), 
                                         data=tcBEIDs_ana, 
                                         # ONE WAY CLUSTERING
                                         cluster="tcBEID",
                                         ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
# wald tests
importing_walds[[i+1]] <- wald(importing_estimates_event[[i+1]], keep="(ti.*er)::-")

etable(importing_estimates_event)


####### Create output tables ###### 
etable(prod_estimates, 
       title="Difference-in-Differences: Productivity estimates",
       label="tab:productivity",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_prod.tex"))

etable(prod_wage_decomp, 
       title="Difference-in-Differences: Wage decomposition in productivity estimates sample",
       label="tab:productivity_wages",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_prod_wage.tex"))

etable(prod_estimates_worker, 
       title="Difference-in-Differences: Productivity estimates",
       label="tab:productivity_worker",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_prod_worker.tex"))


etable(exporting_estimates_sample, 
       title="Difference-in-Differences: Exports on productivity sample",
       label="tab:exports_sample",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_exporting_sample.tex"))

etable(exporting_estimates_full, 
       title="Difference-in-Differences: Exports",
       label="tab:exports",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_exporting.tex"))

etable(exporting_estimates_event, 
       title="Difference-in-Differences: Exports, full sample, event study estimates",
       label="tab:exports_event",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       extralines = list("\\midrule Pre-trends \\\\P-value"=unlist(lapply(paste0("round(exporting_walds[[",
                                                                                 1:length(exporting_walds),
                                                                                 "]]$p, 4)"), 
                                                                          function(x) eval(parse(text=x))))),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_exporting_event.tex"))

etable(importing_estimates_sample, 
       title="Difference-in-Differences: Imports on productivity sample",
       label="tab:imports_sample",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_importing_sample.tex"))

etable(importing_estimates_full, 
       title="Difference-in-Differences: Imports",
       label="tab:imports",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_importing.tex"))

etable(importing_estimates_event, 
       title="Difference-in-Differences: Imports, full sample, event study estimates",
       label="tab:imports_event",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       extralines = list("\\midrule Pre-trends \\\\P-value"=unlist(lapply(paste0("round(importing_walds[[",
                                                                                 1:length(importing_walds),
                                                                                 "]]$p, 4)"), 
                                                                          function(x) eval(parse(text=x))))),
       se.below = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here, "REV1_1_importing_event.tex"))

