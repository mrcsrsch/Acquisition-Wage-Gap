##################################################################################
#### Hires and Separations analysis ####
##################################################################################
# packages
if (!require("fixest")) install.packages("fixest"); library("fixest")

# fixest setup
#### Output table styles & dict ----
style_tex <- style.tex(main="aer", 
                       model.title="",
                       #var.title="Years since takeover",
                       line.top = "\\toprule", line.bottom = "\\bottomrule",
                       fixef.title="\\midrule Fixed-effects",
                       stats.title="\\midrule", 
                       notes.title="\\medskip \\raggedright \\footnotesize \\emph{Notes:}",
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
  
  `time_to_treatment::-4:treatment_group2::TRUE` = "s=-4", #"$\\delta_{-4}$",
  `time_to_treatment::-3:treatment_group2::TRUE` = "s=-3", #"$\\delta_{-3}$",
  `time_to_treatment::-2:treatment_group2::TRUE` ="s=-2", #"$\\delta_{-2}$",
  `time_to_treatment::-1:treatment_group2::TRUE` = "s=-1", #"$\\delta_{-1}$",
  `time_to_treatment::0:treatment_group2::TRUE` = "s=0", #"$\\delta_{0}$",
  `time_to_treatment::1:treatment_group2::TRUE` = "s=1", #"$\\delta_{1}$",
  `time_to_treatment::2:treatment_group2::TRUE` = "s=2", #"$\\delta_{2}$",
  `time_to_treatment::3:treatment_group2::TRUE` = "s=3") #"$\\delta_{3}$")


# create outputs dir if it does not exist
if (!dir.exists(paste0(map_output, "ana/"))) dir.create(paste0(map_output, "ana/"))
map_out_here <- paste0(map_output, "ana/")

#### load data ---- 
# This is the main analysis data set
tcBEIDs_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))

##################################################################################
##################################################################################
#### Create some vars -----
# calculate shares
setorderv(tcBEIDs_ana, cols=c("tcBEID", "year"))
tcBEIDs_ana[, c("workers.lag", "workerFE.lag", "share_in.lag") :=shift(list(workers, workerFE), n=1, type="lag", fill=NA), by=tcBEID]
tcBEIDs_ana[, share_in := fifelse(is.na(workerFE_in), NA_real_, workers_inmovers/workers)]
tcBEIDs_ana[, share_out := fifelse(is.na(workerFE_out.lag), NA_real_, workers_outmovers.lag/workers)]

# generate some before / after indicators
tcBEIDs_ana[, treatment_post := treatment_group2 & time_takeover>=0]
tcBEIDs_ana[, post := time_takeover>=0]
tcBEIDs_ana[, matching_group.post :=paste0(matching_group, post)]

# some vars
tcBEIDs_ana[, workerFE_diff := workerFE-workerFE.lag] # year-on-year change in worker fixed effects
tcBEIDs_ana[, workerFE_in_diff := workerFE_in-workerFE.lag] # excess quality of new hires
tcBEIDs_ana[, workerFE_out_diff := workerFE_out.lag-workerFE.lag] # excress quality of separations


##### define subset ----
tcBEIDs_ana[, keep := time_takeover>=-3 & time_takeover<=3]

# get rows where hire or separation happens
tcBEIDs_ana[keep==TRUE, keep := (!is.na(workerFE_in_diff) & !is.na(workerFE_out_diff)), by=tcBEID]

# get matching groups with hires and separations in pre and post period for each firm
tcBEIDs_ana[keep==TRUE, keep := ((sum(!is.na(workerFE_in_diff) & treatment_group2 & !post)>0) &
                                (sum(!is.na(workerFE_in_diff) & treatment_group2 & post)>0) &
                                (sum(!is.na(workerFE_in_diff) & !treatment_group2 & !post)>0) &
                                (sum(!is.na(workerFE_in_diff) & !treatment_group2 & post)>0) & 
                                  (sum(!is.na(workerFE_out_diff) & treatment_group2 & !post)>0) &
                                  (sum(!is.na(workerFE_out_diff) & treatment_group2 & post)>0) &
                                  (sum(!is.na(workerFE_out_diff) & !treatment_group2 & !post)>0) &
                                  (sum(!is.na(workerFE_out_diff) & !treatment_group2 & post)>0)), by=matching_group]


##### Run regressions (decomposition and the two hiring margins) ----

tcBEIDs_ana[keep==TRUE, workerFE_in_pre := workerFE_in_diff * mean(share_in[!post], na.rm=TRUE), by=tcBEID]
tcBEIDs_ana[keep==TRUE, share_in_pre := share_in * mean(workerFE_in_diff[!post], na.rm=TRUE), by=tcBEID]
tcBEIDs_ana[keep==TRUE, workerFE_out_pre := workerFE_out_diff * mean(share_out[!post], na.rm=TRUE), by=tcBEID]
tcBEIDs_ana[keep==TRUE, share_out_pre := share_out * mean(workerFE_out_diff[!post], na.rm=TRUE), by=tcBEID]

dep.vars <- c("workerFE_diff", "workerFE_in_diff * share_in", "workerFE_out_diff * share_out", "workerFE_in", "share_in")

first_decompo <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  # add dep.var to list
  first_decompo[[i]]$dep.var <- dep.vars[i]
  
  # calculate regular estimate 
  first_decompo[[i]]$res_naive <- feols(as.formula(paste0(dep.vars[i], 
                                                          " ~ treatment_post| tcBEID + matching_group.post")), 
                                        data=tcBEIDs_ana[keep==TRUE, ], 
                                        # ONE WAY CLUSTERING
                                        cluster="tcBEID",
                                        dof = dof(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
}


# output to tex
etable(lapply(paste0("first_decompo[[",1:length(first_decompo), "]]$res_naive"), function(x) eval(parse(text=x))), 
       title="Hires \\& Separations",
       label="tab:hires_separations",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="s5",
       digits.stats="r4",
       # agg="(ti.*nt)::(-?[[:digit:]]):all", 
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.") ,
       sdBelow = TRUE,
       tex=FALSE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here,"table_hires_separations.tex"))
