##################################################################################
# This script retrieves consumer price index (CBS) to deflate prices 
# Base year 2018
##################################################################################
#### packages ######
library(cbsodataR)

#### output dirs #####
if (!dir.exists(paste0(map_data, "step1_basics/"))) dir.create(paste0(map_data, "step1_basics/"))
map_output_here <- paste0(map_data, "step1_basics/")
##################################################################################
##################################################################################

## load CPI index
cpi <- setDT(get_data('83131ned'))
cpi <- cpi[substr(Perioden,1,4) %in% seq(2006,2018,1) & substr(Perioden, 6, 6)=="" &
             Bestedingscategorieen == '000000 Alle bestedingen', 
           .(Perioden, CPIAfgeleid_2)]
cpi[, CPIAfgeleid_2 := as.numeric(CPIAfgeleid_2)]

# define base year and factors 
base_factor <- cpi[Perioden=="2018", CPIAfgeleid_2]
cpi[, factor := base_factor/CPIAfgeleid_2]
cpi[, CPIAfgeleid_2:=NULL]

# some formatting
cpi[, year:= as.integer(paste0(Perioden))]
cpi[, Perioden := NULL]

# save
saveRDS(cpi, file=paste0(map_output_here, "cpi_year.rds"))     