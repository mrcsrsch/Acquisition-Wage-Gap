##################################################################################
#### Cross-sectional comparisons ####
##################################################################################
#### required packages ----
if (!require("fixest")) install.packages("fixest"); library("fixest")
if (!require("xtable")) install.packages("xtable"); library("xtable")

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

# create outputs dir if it does not exist
if (!dir.exists(paste0(map_output, "ana/"))) dir.create(paste0(map_output, "ana/"))
map_out_here <- paste0(map_output, "ana/")

#### load data ----
# load full firm-level dataset
tcBEIDs_full <- readRDS(paste0(map_data,"step9/tcBEIDs_year_numeric.rds")) # this includes all firm years that are in the full network
# load full network
RINPs_year <- readRDS(paste0(map_data,"step9/RINPs_year_numeric.rds"))
# load PSM matched dataset 
tcBEIDs_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))

##################################################################################
##################################################################################

#################################################
#### prepare data #####
# add age profile and # of workers to tcBEIDs_full
tt <- RINPs_year[, .(age_profile = mean(age.profile), workers=.N, year_fe = year_fe[1]), by=tcBEID_year]
tt[, .N]
tcBEIDs_full[, .N]
tcBEIDs_full <- merge(tcBEIDs_full, tt, by="tcBEID_year")
tcBEIDs_full[, .N]
rm(RINPs_year, tt)

# remove domestic MNEs (and firms that ever are domestic MNEs)
# this is BULA_DOCHTERS==TRUE & UCI == NL
tcBEIDs_full[, DMNE := any(BULA_DOCHTERS==TRUE & bui_mul==FALSE), by=tcBEID]
tcBEIDs_full <- tcBEIDs_full[DMNE==F, !c("DMNE")]


# sample: selection 
##### continuously observed ######
tcBEIDs_full[, sample_cont := all(diff(year)==1), by=tcBEID]
##### firms with >= 5 employees #####
tcBEIDs_full[, sample_workers := all(workers>=5), by=tcBEID]

# only keep selected firms
tcBEIDs_full <- tcBEIDs_full[sample_cont==T & sample_workers==T,]


##### identify control firms (always domestic) ####
tcBEIDs_full[, control_firm := all(bui_mul==F), by=tcBEID]

##### identify always MNEs ####
tcBEIDs_full[, always_MNE := all(bui_mul==T), by=tcBEID]

####################################################################################

# x-sec comparison of matched acquired firms to all domestic firms 
## get acquired firms over post-acquisition years
tfirms <- tcBEIDs_ana[treatment_group2==T & time_takeover >= 0 & time_takeover <= 3, unique(tcBEID_year)]
## get all domestic firms (control firms)
cfirms <- tcBEIDs_full[control_firm==T, unique(tcBEID_year)] 

## create new panel (matched after acquistion vs. all domestic)
matchedvsall <- tcBEIDs_full[tcBEID_year %in% c(tfirms, cfirms), ]
### only keep years where treated firms are present
matchedvsall[, keep := year %in% matchedvsall[tcBEID_year%in%tfirms, unique(year)]]
matchedvsall <- matchedvsall[keep==T, !c("keep")]
## add id for treated
matchedvsall[, treated := tcBEID_year %in% tfirms]

## create new panel (always foreign + acquisition target vs. always domestic)
## get firm years that are targets of acquistions
## get acquired firms over pre-acquisition years
tbfirms <- tcBEIDs_ana[treatment_group2==T & time_takeover < 0, unique(tcBEID_year)] 
## get firms that are always MNEs
afirms <- tcBEIDs_full[always_MNE==T, unique(tcBEID_year)] 

# create panel 
prevsall <- tcBEIDs_full[tcBEID_year %in% c(tbfirms, afirms, cfirms),]
# add id for to-be-acquired and always
prevsall[, to_be_acquired := tcBEID_year %in% tbfirms]
prevsall[, always := tcBEID_year %in% afirms]


#### Regressions: X-sec domp: matched acquired vs. all other domestic firms #####
dep.vars <- c("mean_lrhwage", "firm_FE",  "workerFE", "age_profile")

decomp_matchedvsall <- list()

for (i in 1:length(dep.vars)){
  # Full 
  decomp_matchedvsall[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                   " ~ treated | year^nace_2digit")), 
                                 data=matchedvsall,
                                 # ONE WAY CLUSTERING
                                 cluster="tcBEID",
                                 ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
}

etable(decomp_matchedvsall)


#### Regressions: X-sec domp: matched acquired vs. matched domestic firms #####
dep.vars <- c("mean_lrhwage", "firm_FE",  "workerFE", "age_profile")

decomp_matchedvsmatched <- list()

for (i in 1:length(dep.vars)){
  # Full 
  decomp_matchedvsmatched[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                      " ~ treatment_group2 | year^nace_2digit")), 
                                    data=tcBEIDs_ana[time_takeover>=0 & time_takeover <= 3,],
                                    # ONE WAY CLUSTERING
                                    cluster="tcBEID",
                                    ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
}

etable(decomp_matchedvsmatched)


#### Regressions: X-sec domp: always foreign / to-be-acquired vs. always domestic #####
dep.vars <- c("mean_lrhwage", "firm_FE",  "workerFE", "age_profile")

decomp_prevsall <- list()

for (i in 1:length(dep.vars)){
  # Full 
  decomp_prevsall[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                          " ~ to_be_acquired + always | year^nace_2digit")), 
                                        data=prevsall,
                                        # ONE WAY CLUSTERING
                                        cluster="tcBEID",
                                        ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
}

etable(decomp_prevsall)


### save output tables #### 

#### Output tables ##### 
# matched post-acquisition vs. all domestic firms
etable(decomp_matchedvsall,
       title="Cross-sectional wage decomposition, matched post-acquisition vs. always domestic firms.",
       label="tab:xsec_matchedpostvsall",
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
       file=paste0(map_output_here, "table_xsec_matchedposvsall.tex"))

# matched post-acquisition vs. matched control
etable(decomp_matchedvsmatched,
       title="Cross-sectional wage decomposition, matched post-acquisition vs. matched control firms.",
       label="tab:xsec_matchedvsmatched",
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
       file=paste0(map_output_here, "table_xsec_matchedvsmatched.tex"))

# matched pre-acquisition and always foreign vs. all domestic
etable(decomp_prevsall,
       title="Cross-sectional wage decomposition, pre-acquisition domestic and always foreign vs. always domestic firms.",
       label="tab:xsec_decomp_prevsall",
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
       file=paste0(map_output_here, "table_xsec_decomp_prevsall.tex"))


