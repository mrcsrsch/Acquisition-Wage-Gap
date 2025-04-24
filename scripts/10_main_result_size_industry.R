##################################################################################
### Wage decomposition, overall, by firm size, by industry ####
##################################################################################

#### packages ---
if (!require("fixest")) install.packages("fixest"); library("fixest")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr"); library("ggpubr")

if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")


if (!require("data.table")) install.packages("data.table"); library("data.table")

# create outputs dir if it does not exist
if (!dir.exists(paste0(map_output, "ana/"))) dir.create(paste0(map_output, "ana/"))
map_out_here <- paste0(map_output, "ana/")

#### Read in data ----
# This is the main analysis data set
tcBEIDs_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))

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

##################################################################################
##################################################################################

##################################################################################
#### Wage Decomposition (Main) ----
dep.vars <- c("log_wage", "mean_lrhwage", "firm_FE",  "workerFE", "age_profile")

wage_decomposition <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  # add dep.var to list
  wage_decomposition[[i]]$dep.var <- dep.vars[i]
  
  #### Event time frame ----
  
  # get rows that lie within event frame
  tcBEIDs_ana[, subset:=(time_takeover>=-3 & time_takeover<=3)]
  
  # for these rows check if dep.var is NA for observations observation within a matching_group at t=-1 (reference year)
  # if so, remove the entire group
  #rows_delete <- tcBEIDs_ana[rows, .I[any(is.na(eval(as.symbol(dep.vars[i]))[which(time_takeover==-1)])) & (is.logical(treatment_group2))], by=matching_group][, V1]
  tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[i]))[time_takeover==-1])), by=matching_group]

  # whereever dep.var is NA in for any obs in matching group at t, remove t for matching group from analysis
  tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[i])))), by=c("time_takeover", "matching_group")]
  
  # turn rows vector into logical vector
  wage_decomposition[[i]]$subset <- tcBEIDs_ana[, subset]

  # calculate regular estimate 
  wage_decomposition[[i]]$res_naive <- feols(as.formula(paste0(dep.vars[i], 
                                                          " ~ i(time_to_treatment, f2=treatment_group2, drop=c(-1, -1000)) | tcBEID + matching_group.year")), 
                                        data=tcBEIDs_ana, 
                                        subset= wage_decomposition[[i]]$subset,
                                        # ONE WAY CLUSTERING
                                        cluster="tcBEID",
                                        dof = dof(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
  
  tcBEIDs_ana[, subset := NULL]
  
  # calculate pre-trend wald statistic
  wage_decomposition[[i]]$wald <- wald(wage_decomposition[[i]]$res_naive, keep="(ti.*nt)::-")
}

### Output table ###
etable(list(wage_decomposition[[2]]$res_naive,
			wage_decomposition[[3]]$res_naive, wage_decomposition[[4]]$res_naive,
			wage_decomposition[[5]]$res_naive), 
       title="Difference-in-Differences: Log wage decomposition",
       label="tab:wage_decompo",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
      # agg="(ti.*nt)::(-?[[:digit:]]):all", 
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
				 "Pre-trends shows the p-value of a Wald test on the joint significance of pre-acquistions effects (s=-3 \\& s=-2).",
				 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
      # Add Wald statistic with stars
      extraline = list("{title:\\midrule Pre-trends; where:stats}P-value"=c(
                                                                         paste0(round(wage_decomposition[[2]]$wald$p, 4), 
                                                                                ifelse(wage_decomposition[[2]]$wald$p<=0.001, "***",
                                                                                       ifelse(wage_decomposition[[2]]$wald$p<=0.01, "**",
                                                                                              ifelse(wage_decomposition[[2]]$wald$p<=0.05, "*",
                                                                                                     ifelse(wage_decomposition[[2]]$wald$p<=0.1, ".", ""))))),
                                                                         paste0(round(wage_decomposition[[3]]$wald$p, 4), 
                                                                                ifelse(wage_decomposition[[3]]$wald$p<=0.001, "***",
                                                                                       ifelse(wage_decomposition[[3]]$wald$p<=0.01, "**",
                                                                                              ifelse(wage_decomposition[[3]]$wald$p<=0.05, "*",
                                                                                                     ifelse(wage_decomposition[[3]]$wald$p<=0.1, ".", ""))))),
                                                                          paste0(round(wage_decomposition[[4]]$wald$p, 4),
                                                                                                                     ifelse(wage_decomposition[[4]]$wald$p<=0.001, "***",
                                                                                                                           ifelse(wage_decomposition[[4]]$wald$p<=0.01, "**",
                                                                                                                                 ifelse(wage_decomposition[[4]]$wald$p<=0.05, "*",
                                                                                                                                       ifelse(wage_decomposition[[4]]$wald$p<=0.1, ".", ""))))),
																		  paste0(round(wage_decomposition[[5]]$wald$p, 4),
                                                                                                                     ifelse(wage_decomposition[[5]]$wald$p<=0.001, "***",
                                                                                                                           ifelse(wage_decomposition[[5]]$wald$p<=0.01, "**",
                                                                                                                                 ifelse(wage_decomposition[[5]]$wald$p<=0.05, "*",
                                                                                                                                       ifelse(wage_decomposition[[5]]$wald$p<=0.1, ".", "")))))
																									 )),
       sdBelow = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here,"table_wage_decompo.tex"))


###### Create plots ------

# Worker vs. firm fixed effects
pdf(file=paste0(map_output_here, "plot_main_comparison.pdf"), width=4*1.9, height=3*1.9)

iplot(wage_decomposition[[2]]$res_naive, 
      main = "",
      ci_level=0.95, grid=TRUE, 
      x.shift=-0.225,
      pt.pch=21,
      grid.par = list(lty=0, col="gray"),
      xlab="Years since acquisition")
iplot(wage_decomposition[[3]]$res_naive, add=TRUE,
      x.shift=-0.075,
      grid.par = list(lty=0, col="gray"),
      pt.pch=22,
      ci_level=0.95)
iplot(wage_decomposition[[4]]$res_naive, add=TRUE,
      x.shift=0.075,
      grid.par = list(lty=0, col="gray"),
      pt.pch=24,
      ci_level=0.95)
iplot(wage_decomposition[[5]]$res_naive, add=TRUE,
      main = "Worker observables (firm-level mean)",
      x.shift=0.225,
      ci_level=0.95, grid=TRUE, 
      grid.par = list(lty=0, col="gray"),
      pt.pch=25,
      xlab="Years since acquisition")
legend("topleft", 
       pch=c(21,22,24,25), legend=c("ln Wage" , "Firm FE", "Worker FE", "Age profile"))
dev.off()



##################################################################################
#### Wage Decomposition (Firm Size) ----
dep.vars <- c("firm_FE", "workerFE") #c("mean_lrhwage", "firm_FE",  "workerFE", "age_profile")

# find size classes of treatment firms
tcBEIDs_ana[, empl_size := cut(workers[treatment_group2 & time_takeover==-1], breaks=c(5,20,50,100,Inf), right=FALSE, 
                               labels=c("5 - 19", "20 - 49", "50-99", "> 100")), by=matching_group]


size <- c("5 - 19", "20 - 49", "50-99", "> 100")

wage_decomposition_size <- vector(mode="list", length=length(dep.vars)*length(size))

i <- 0

for (p in 1:length(size)){
  for (f in 1:length(dep.vars)) {
    i <- i+1
    # add dep.var to list
    wage_decomposition_size[[i]]$dep.var <- dep.vars[f]
    
    # add industry to list
    wage_decomposition_size[[i]]$size <- size[p]
    
    
    #### Event time frame ----
    
    # get rows that lie within event frame 
    tcBEIDs_ana[, subset:=(time_takeover>=-3 & time_takeover<=3)]
    
    # get rows that lie within industry
    tcBEIDs_ana[subset==TRUE, subset:= (empl_size[time_takeover==-1]==size[p]), by=tcBEID]
  
    
    # for these rows check if dep.var is NA for observations observation within a matching_group at t=-1 (reference year)
    # if so, remove the entire group
    #rows_delete <- tcBEIDs_ana[rows, .I[any(is.na(eval(as.symbol(dep.vars[i]))[which(time_takeover==-1)])) & (is.logical(treatment_group2))], by=matching_group][, V1]
    tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[f]))[time_takeover==-1])), by=matching_group]
    
    # whereever dep.var is NA in for any obs in matching group at t, remove t for matching group from analysis
    tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[f])))), by=c("time_takeover", "matching_group")]
    
    # turn rows vector into logical vector
    wage_decomposition_size[[i]]$subset <- tcBEIDs_ana[, subset]
    tcBEIDs_ana[, subset := NULL]
    
    # calculate regular estimate 
    wage_decomposition_size[[i]]$res_naive <- feols(as.formula(paste0(dep.vars[f], 
                                                                          " ~ i(time_to_treatment, f2=treatment_group2, drop=c(-1, -1000)) | tcBEID + matching_group.year")), 
                                                        data=tcBEIDs_ana, 
                                                        subset= wage_decomposition_size[[i]]$subset,
                                                        # ONE WAY CLUSTERING
                                                        cluster="tcBEID",
                                                        dof = dof(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))

    # calculate pre-trend wald statistic
    wage_decomposition_size[[i]]$wald <- wald(wage_decomposition_size[[i]]$res_naive, keep="(ti.*nt)::-")
    
    wage_decomposition_size[[i]]$tab <- data.table(industry=size[p],
                                                       type=dep.vars[f],
                                                       time_takeover=c(-3,-2,0,1,2,3),
                                                       coef=wage_decomposition_size[[i]]$res_naive$coefficients,
                                                       se=wage_decomposition_size[[i]]$res_naive$se,
                                                       pval=wage_decomposition_size[[i]]$res_naive$coeftable[,4],
                                                       obs=wage_decomposition_size[[i]]$res_naive$nobs)
  }
}

# Output to tex
### Output table ###
etable(lapply(paste0("wage_decomposition_size[[",1:length(wage_decomposition_size), "]]$res_naive"), function(x) eval(parse(text=x))), 
       title="Size comparison",
       label="tab:size",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       # agg="(ti.*nt)::(-?[[:digit:]]):all", 
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Ln is the the natural logarithm.",
                 "Pre-trends shows the p-value of a Wald test on the joint significance of pre-acquistions effects (s=-3 \\& s=-2).",
                 "Mean wage-age pr refers to the mean log wage component associated with a third-order polynomial in age-40."),
       extraline = list("{title:\\midrule Pre-trends; where:stats}P-value"=unlist(lapply(paste0("round(wage_decomposition_size[[",
                                                                                                1:length(wage_decomposition_size),
                                                                                                "]]$wald$p, 4)"), 
                                                                                         function(x) eval(parse(text=x))))),
       sdBelow = TRUE,
       tex=FALSE,
       style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_output_here,"table_size.tex"))





##################################################################################
#### Wage Decomposition (Industry) ----
# define industry classifications
# knowledge intensive services (KIS)
KIS <- c(50, 51, 58:63, 64:66, 69:75, 78, 80, 84:93)

# less knowledge intensive services (LKIS)
LKIS <- c(45:47, 49, 52:53, 55, 56, 68, 77, 79, 81, 82, 94:96, 97:99)

# high technology
HT <- c(21, 26)

# medium-high-technology
MHT <- c(20, 27:30)

# medium-low-technology
MLT <- c(19, 22:25, 33)

# low technology 
LT <- c(10:18, 31:32)

# add to tcBEIDs_ana
tcBEIDs_ana[substr(nace, 1, 2) %in% KIS, industry_class := "KIS"]
tcBEIDs_ana[substr(nace, 1, 2) %in% LKIS, industry_class := "LKIS"]
tcBEIDs_ana[substr(nace, 1, 2) %in% HT, industry_class := "HT"]
tcBEIDs_ana[substr(nace, 1, 2) %in% MHT, industry_class := "HT"]
tcBEIDs_ana[substr(nace, 1, 2) %in% MLT, industry_class := "LT"]
tcBEIDs_ana[substr(nace, 1, 2) %in% LT, industry_class := "LT"]
tcBEIDs_ana[is.na(industry_class), industry_class := "REST"]


# per matching group, find out if industry_class differs, if it does assign to REST
tcBEIDs_ana[, industry_class := fifelse(industry_class[treatment_group2 & time_takeover==-1]==industry_class[!treatment_group2 & time_takeover==-1], 
                                       industry_class[treatment_group2 & time_takeover==-1],  "REST"), by=matching_group]


# run regression
dep.vars <- c("firm_FE", "workerFE") #c("mean_lrhwage", "firm_FE",  "workerFE", "age_profile")
industry <- c("KIS", "LKIS", "HT", "LT") #, "REST") #, "M", "J")


wage_decomposition_industry <- vector(mode="list", length=length(dep.vars)*length(industry))

i <- 0

for (p in 1:length(industry)){
  for (f in 1:length(dep.vars)) {
    i <- i+1
    # add dep.var to list
    wage_decomposition_industry[[i]]$dep.var <- dep.vars[f]
    
    # add industry to list
    wage_decomposition_industry[[i]]$industry <- industry[p]
    
    
    #### Event time frame ----
    
    # get rows that lie within event frame 
    tcBEIDs_ana[, subset:=(time_takeover>=-3 & time_takeover<=3)]
    
    # get rows that lie within industry class
      tcBEIDs_ana[subset==TRUE, subset:= (industry_class[time_takeover==-1]==industry[p]), by=tcBEID]
    
    # for these rows check if dep.var is NA for observations observation within a matching_group at t=-1 (reference year)
    # if so, remove the entire group
    #rows_delete <- tcBEIDs_ana[rows, .I[any(is.na(eval(as.symbol(dep.vars[i]))[which(time_takeover==-1)])) & (is.logical(treatment_group2))], by=matching_group][, V1]
    tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[f]))[time_takeover==-1])), by=matching_group]
    
    # whereever dep.var is NA in for any obs in matching group at t, remove t for matching group from analysis
    tcBEIDs_ana[subset==TRUE, subset := !any(is.na(eval(as.symbol(dep.vars[f])))), by=c("time_takeover", "matching_group")]
    
    # only matches from 2-digit industries
   # tcBEIDs_ana[subset==TRUE, subset := substr(matching_group,1,2)=="22"]
    
    # turn rows vector into logical vector
    wage_decomposition_industry[[i]]$subset <- tcBEIDs_ana[, subset]
    tcBEIDs_ana[, subset := NULL]
    
    # calculate regular estimate 
    wage_decomposition_industry[[i]]$res_naive <- feols(as.formula(paste0(dep.vars[f], 
                                                                          " ~ i(time_to_treatment, f2=treatment_group2, drop=c(-1, -1000)) | tcBEID + matching_group.year")), 
                                                        data=tcBEIDs_ana, 
                                                        subset= wage_decomposition_industry[[i]]$subset,
                                                        # ONE WAY CLUSTERING
                                                        cluster="tcBEID",
                                                        dof = dof(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
    
    # calculate pre-trend wald statistic
    wage_decomposition_industry[[i]]$wald <- wald(wage_decomposition_industry[[i]]$res_naive, keep="(ti.*nt)::-")
    
    wage_decomposition_industry[[i]]$tab <- data.table(industry_class=industry[p],
                                                       type=dep.vars[f],
                                                       time_takeover=c(-3,-2,0,1,2,3),
                                                       coef=wage_decomposition_industry[[i]]$res_naive$coefficients,
                                                       se=wage_decomposition_industry[[i]]$res_naive$se,
                                                       pval=wage_decomposition_industry[[i]]$res_naive$coeftable[,4],
                                                       obs=wage_decomposition_industry[[i]]$res_naive$nobs)
  }
}

# Output to tex
### Output table ###
etable(lapply(paste0("wage_decomposition_industry[[",1:length(wage_decomposition_industry), "]]$res_naive"), function(x) eval(parse(text=x))), 
       title="Industry comparison (HI/LO)",
       label="tab:industry_classes",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       # agg="(ti.*nt)::(-?[[:digit:]]):all", 
       cluster="tcBEID", #  
       dict = mydict,
       fitstat=c("n", "r2"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       notes = c("***=0.001, **=0.01, *=0.05, .=0.1",
                 "Clustered standard errors (Firm ID) in parantheses.",
                 "Pre-trends shows the p-value of a Wald test on the joint significance of pre-acquisition effects (s=-3 \\& s=-2)."),
       # Add Wald statistic with stars
       extraline = list("{title:\\midrule Pre-trends; where:stats}P-value"=unlist(lapply(paste0("round(wage_decomposition_industry[[",
                                                                                                1:length(wage_decomposition_industry),
                                                                                                "]]$wald$p, 4)"), 
                                                                                         function(x) eval(parse(text=x))))),
       sdBelow = TRUE,
       tex=TRUE,
       style.tex = style_tex,
       replace=TRUE,
 file=paste0(map_output_here, "able_industry_classes.tex"))
