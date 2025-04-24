##################################################################################
# Fit AKM with time variant fixed effects
##################################################################################

### packages ####
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

### output dir ####
if (!dir.exists(paste0(map_data, "step6/"))) dir.create(paste0(map_data, "step6/"))
if (!dir.exists(paste0(map_data, "step6/yeartcBEID/"))) dir.create(paste0(map_data, "step6/yeartcBEID/"))
map_output_here <- paste0(map_data, "step6/yeartcBEID/")

### Read in yeartcBEID network #####
RINPs_year <- readRDS(paste0(map_data,"step5/yeartcBEID_network.rds"))
# only keep necessary vars
RINPs_year <- RINPs_year[, c("RINP", "tcBEIDyear", "year", "SBEID", "tcBEID", "lrhwage", "age")]
gc()

##################################################################################
##################################################################################

#### Plot wage-age profile ----
ggplot(data=RINPs_year[, .(mean_lrhwage = mean(lrhwage)), by=age], aes(x=age, y=mean_lrhwage)) +
  geom_point() +
  xlab("Age") + ylab("Mean ln wage") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5, margin=margin(-15,0,0,0)))
  if (!dir.exists(paste0(map_data, "outputs/"))) dir.create(paste0(map_data, "outputs/"))
ggsave(paste0(map_data,"outputs/plot_wage_age_profile.pdf"), plot=last_plot())


#### Demean data with age controls and year FEs ----

# create vars
RINPs_year[,c("age_1", "age_2", "age_3") := list(as.integer(age-40), as.integer((age-40)^2), as.integer((age-40)^3))]

# fit model
fit <- lm(lrhwage ~ age_1 + age_2 + age_3 + factor(year), data=RINPs_year, model=FALSE)
gc()

saveRDS(fit, paste0(map_output_here,"step6/yeartcBEID/fit_age_year.rds"), compress=TRUE)

# add residuals to RINPs_year
RINPs_year[, resid_1:=fit$residuals]
RINPs_year[, c("age_1", "age_2", "age_3") := NULL]
rm(fit)

saveRDS(RINPs_year, paste0(map_output_here,"step6/yeartcBEID/RINPs_year_before_AKM.rds"), compress=TRUE)

rm(RINPs_year)
gc()


