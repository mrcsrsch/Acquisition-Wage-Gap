##################################################################################
# Find largest connected set to estimate firm and worker fes
##################################################################################

#### Packages ----
#if (!require("igraph")) install.packages("igraph"); library("igraph")
if (!require("lfe")) install.packages("lfe"); library("lfe")

#### Read in data ----
# Read in full data set with numeric IDs
RINPs_year <- readRDS(paste0(map_data,"step4/RINP_tcBEID.rds"))

#### output dirs #####
if (!dir.exists(paste0(map_data, "step5/"))) dir.create(paste0(map_data, "step5/"))
map_output_here <- paste0(map_data, "step5/")

##################################################################################
##################################################################################
#### create tcBEIDyear ----
RINPs_year[, tcBEIDyear := factor(paste(tcBEID, "-", year))]

#### Subset to largest connected set (tcBEIDyear) ----
# find largest connected set of year_firm_FEs
RINPs_year[, connected.set := compfactor(list(RINP=RINP, tcBEIDyear=tcBEIDyear), WW=FALSE)]
RINPs_year <- RINPs_year[connected.set==1, !c("connected.set")]

# Save largest connected set
saveRDS(RINPs_year, paste0(map_output_here,"yeartcBEID_network.rds"), compress=TRUE)
