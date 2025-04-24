##################################################################################
#### Diff-in-Diff on unmatched sample ###
##################################################################################


#### packages ---
if (!require("fixest")) install.packages("fixest"); library("fixest")

  # create outputs dir if it does not exist
if (!dir.exists(paste0(map_output, "ana/"))) dir.create(paste0(map_output, "ana/"))
map_out_here <- paste0(map_output, "ana/")

#### load data ---
tcBEIDs_full <- readRDS(paste0(map_data,"step9/tcBEIDs_year_numeric.rds")) # this includes all firm years that are in the full network
# load full network
RINPs_year <- readRDS(paste0(map_data,"step9/RINPs_year_numeric.rds"))
# load PSM matched dataset 
tcBEIDs_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))

##################################################################################
##################################################################################
# add age profile and post-indicator
tcBEIDs_full <- merge(tcBEIDs_full, RINPs_year[, .(age.profile=mean(age.profile)), by=.(tcBEID, year)], by=c("tcBEID", "year"))
tcBEIDs_full[, post := treatment_group & time_takeover >=0]

# select sample
tcBEIDs_full[, sample := (treatment_group & time_takeover %in% -3:3) | !treatment_group]
tcBEIDs_full[is.na(time_takeover), time_takeover := -1]
# run DiD on full sample, without matching 
dep.vars <- c("mean_lrhwage", "firm_FE",  "workerFE", "age.profile")

wage_full <- vector(mode="list", length=length(dep.vars))

for (i in 1:length(dep.vars)){
  # only keep industry-years with a treated and a control firm
  
  wage_full[[i]] <- feols(as.formula(paste0(dep.vars[i], 
                                                    " ~ post + i(factor_var = time_takeover, var=treatment_group, ref=c(-1)) | tcBEID + nace_2digit^year")), 
                                  data=tcBEIDs_full,
                                  subset = ~ sample,
                                  # ONE WAY CLUSTERING
                                  cluster="tcBEID",
                                  ssc = ssc(fixef.K="full", adj=FALSE, cluster.adj=FALSE, t.df="min"))
}

etable(wage_full)


etable(wage_full, 
       title="Difference-in-Differences: Log wage decomposition, unmatched sample",
       label="tab:unmatched",
       float=TRUE,
       fixef_sizes = TRUE,
       depvar=TRUE,
       digits="r4",
       digits.stats="r4",
       cluster="tcBEID", #  
       #dict = mydict,
       fitstat=c("n", "r2"),
       signifCode = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1),
       sdBelow = TRUE,
       tex=TRUE,
      # style.tex = style_tex,
       replace=TRUE,
       file=paste0(map_out_here,"table_wage_decompo_unmatched.tex"))
