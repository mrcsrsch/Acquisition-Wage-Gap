##################################################################################
### Summary Statistics of matched sample ####
##################################################################################

#### required packages ----
if (!require("ggplot2"))  install.packages("ggplot2"); library("ggplot2")
if (!require("vtable"))  install.packages("vtable"); library("vtable")

# create outputs dir if it does not exist
if (!dir.exists(paste0(map_output, "ana/"))) dir.create(paste0(map_output, "ana/"))
if (!dir.exists(paste0(map_output, "ana/descriptives/"))) dir.create(paste0(map_output, "ana/descriptives/"))
map_out_here <- paste0(map_output, "ana/descriptives/")

##################################################################################
##################################################################################

#### plot size class and industry (matched sample) ----
#### load data (matchted data set) ----
tcBEIDs_year_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_ana_numeric.rds"))


# identify relevant nace21 industries
tcBEIDs_year_ana[, nace_cats := fifelse(nace21=="C", "Manufacturing", 
                                        fifelse(nace21=="G", "Wholesale and\nRetail Trade",
                                                fifelse(nace21=="M", "Professional, Scientific\nand Technical Activities",
                                                        "Other")))]
# factor and set order for graph
tcBEIDs_year_ana[, nace_cats := factor(nace_cats)]
tcBEIDs_year_ana[, nace_cats := factor(nace_cats, levels=c("Other", 
                                                           "Professional, Scientific\nand Technical Activities",
                                                           #   "Information and\nCommunication",
                                                           "Manufacturing", 
                                                           "Wholesale and\nRetail Trade"))]

# identify worker size classes
tcBEIDs_year_ana[time_takeover==-1, empl_size := cut(workers, breaks=c(5,20,50,100,250, Inf), right=FALSE, 
                                                    labels=c("5 - 19", "20 - 49", "50-99", "100 - 249", ">249"))]
# factor and order for graph
tcBEIDs_year_ana[, empl_size := factor(empl_size)]
tcBEIDs_year_ana[, empl_size := factor(empl_size, levels=c("5 - 19", "20 - 49", "50-99", "100 - 249", ">249"))]


# draw plot
ggplot(data=tcBEIDs_year_ana[treatment_group2==TRUE & time_takeover==-1, .(obs=.N), by=c("empl_size", "nace_cats")], aes(x=empl_size, y=obs, fill=nace_cats)) +
  #geom_bar(stat="identity") +
  geom_col(colour="black") +
  scale_fill_grey(start=0.5, end=0.95) + # in grey scale
#  scale_fill_brewer(palette = "Pastel1") +
  ylab("Foreign acquisitions") + xlab("Workers") +
  guides(fill=guide_legend(title="", reverse=TRUE)) +
  scale_y_continuous(breaks=seq(50,1400,50)) +
  theme(axis.text.x=element_text(angle=90)) + 
  theme_set(theme_gray(base_size=14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(-15,0,0,0)), 
        legend.position = "bottom",
        legend.justification = "left",
        legend.direction = "horizontal")
# save plot
if (!dir.exists(paste0(map_data, "outputs/"))) dir.create(paste0(map_data, "outputs/"))
if (!dir.exists(paste0(map_data, "outputs/descriptives/"))) dir.create(paste0(map_data, "outputs/descriptives/"))

ggsave(paste0(map_output_here, "plot_matched_size_nace.pdf"), plot=last_plot(), dpi=600, scale=1.4)


##################################################################################
#### Summary statistics table ---
# matched sample
# only keep observations in analysis time frame
tcBEIDs_year_ana <- tcBEIDs_year_ana[time_takeover>=-3 & time_takeover<=3, ]


# create var that splits up control, and treated before acquired and after acquired
tcBEIDs_year_ana[, gg := fifelse(treatment_group2 & time_takeover<0, "Before Acquisition",
                                 fifelse(treatment_group2 & time_takeover>=0, "After Acquisition", "Non-acquired firms"))]
tcBEIDs_year_ana[, c("Firms", "Firm_years") := list(uniqueN(tcBEID), .N), by=gg]

# unmatched sample
# read in pre-matching data set
tcBEIDs_year_ana <- readRDS(paste0(map_data,"step9/tcBEIDs_year_numeric.rds"))


# first remove foreign firms that are not in the treatment group
tcBEIDs_year_ana[, exclude := (treatment_group==FALSE & (any(bui_mul==TRUE) | any(BULA_DOCHTERS==TRUE))), by=tcBEID]

# create var that splits up potential control, before acquired and after acquired
tcBEIDs_year_ana[, gg := fifelse(treatment_group & time_takeover<0, "To-be-acquired firms",
                                         fifelse(treatment_group & time_takeover>0, "after", 
                                                 fifelse(treatment_group & time_takeover==0, "exclude", "Non-acquired firms")))]

tt <- tcBEIDs_year_ana[exclude==FALSE & (gg=="To-be-acquired firms" | gg=="Non-acquired firms"), .(#firms = uniqueN(tcBEID),
                                                        #firm_years = .N,
                                                        gg=gg,
                                                        tcBEID = tcBEID,
                                                        # Worker level vars
                                                        ln_employment=log_employment,
                                                        diff_ln_employment = log_employment.change,
                                                        #worker_age = mean_age,
                                                        # share_female = share_females,
                                                        # 
                                                        # # Wage components
                                                        mean_log_wage = mean_lrhwage,
                                                        diff_mean_log_wage = mean_lrhwage.change,
                                                        firm_FE = firm_FE,
                                                        diff_firm_FE = firm_FE.change1,
                                                        workerFE = workerFE,
                                                        diff_workerFE = workerFE.change1,
                                                        var_workerFE = workerFE_var,
                                                        export_value=export_value)]
tt[, c("Firms", "Firm_years") := list(uniqueN(tcBEID), .N), by=gg]

labs <- data.table(var=c("gg", "Firms", "Firm_years", "ln_employment", "diff_ln_employment", "worker_age", "mean_log_wage",
                         "diff_mean_log_wage", "firm_FE", "diff_firm_FE", "workerFE", "diff_workerFE", 
                         "var_workerFE", "export_value"), 
                   label=c("", "Firms", "Firm years", "Ln employment", "Ln employment growth", "Worker age", "Mean ln wage", 
                           "Mean ln wage growth", "Firm fixed effect", "Firm fixed effect growth", "Mean worker fixed effect", "Mean worker fixed effect growth",
                           "Variance worker fixed effect", "Export value"))

st(tt, group='gg', summ=c('mean(x)','sd(x)'), 
   vars=c("Firms", "Firm_years", "ln_employment", "diff_ln_employment", "export_value", "\\textit{Wage components}", "mean_log_wage",
          "diff_mean_log_wage", "firm_FE", "diff_firm_FE", "workerFE", "diff_workerFE", 
          "var_workerFE"),
   summ.names=c("Mean", "SD"),
   labels=labs, 
   digits=5, 
   group.test=TRUE, factor.numeric=TRUE,
   title="Firm-level Descriptive statistics") 
