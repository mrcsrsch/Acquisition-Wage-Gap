##################################################################################
# Add firm level data
# My apologies, this script is messy and contains redundant calls.
##################################################################################
### packages ####
if (!require("readxl")) install.packages("readxl"); library("readxl")

### output dir ###
if (!dir.exists(paste0(map_data, "step7/"))) dir.create(paste0(map_data, "step7/"))
map_output_here <- paste0(map_data, "step7/")

### data ###
RINPs_year <- readRDS(paste0(map_data,"step6/yeartcBEID/tcBEIDyear_AKM.rds"))

##################################################################################
##################################################################################

#### Identify per period movers ---
# Identify movers in current year
setorderv(RINPs_year, c("RINP", "year"))
RINPs_year[, c("year.lag", "year.lead", "tcBEID.lag", "tcBEID.lead") := shift(.(year, tcBEID), n=c(1, -1), fill=NA, type="lag"), by=RINP]
RINPs_year[, inmove_mover := (!is.na(tcBEID.lag) & tcBEID!=tcBEID.lag & year.lag==(year-1))]
RINPs_year[, outmove_mover := (!is.na(tcBEID.lead) & tcBEID!=tcBEID.lead & year.lead==(year+1))]
RINPs_year[, inmove_NA :=  (is.na(tcBEID.lag) | year.lag!=(year-1))]
RINPs_year[, outmove_NA := (is.na(tcBEID.lead) | year.lead!=(year+1))]

# adjust edges of panel
RINPs_year[year==2006, inmove_NA := FALSE]
RINPs_year[year==2018, outmove_NA := FALSE]

#### Identify movers in panel ---
# find movers 
# Idea create a data.table with unique RINP, tcBEID tuples, distinguish movers and stayers
movers <- unique(RINPs_year[, c("RINP", "tcBEID")
                              ])[, .(firms=.N), by=RINP
                                ][, mover:=fifelse(firms>1, TRUE, FALSE)]
# merge back to main data.table
RINPs_year <- merge(RINPs_year, movers, by=c("RINP"))
rm(movers)
gc()

#### Aggregate information to firm level ---

k <- length(names(RINPs_year))

setkeyv(RINPs_year, c("tcBEID", "year"))
timer <- Sys.time()
RINPs_year[, `:=` (workerFE_mover = mean(worker_FE[mover]),
                   workerFE_stayer = mean(worker_FE[!mover]),
                   rhwage_mover = mean(exp(lrhwage[mover])),
                   rhwage_stayer = mean(exp(lrhwage[!mover])),
                   rhwage = mean(exp(lrhwage)),
                   resid_1_mean = mean(resid_1),
                   workerFE=mean(worker_FE), 
                   #workerFE_sd = sd(worker_FE),
                   workerFE_moverin = mean(worker_FE[inmove_mover]),
                   workerFE_moverout = mean(worker_FE[outmove_mover]),
                   workerFE_NAin = mean(worker_FE[inmove_NA]),
                   workerFE_NAout = mean(worker_FE[outmove_NA]),
                   workerFE_in = mean(worker_FE[inmove_mover | inmove_NA]),
                   workerFE_out = mean(worker_FE[outmove_mover | outmove_NA ]),
                   ins_mover = sum(inmove_mover),
                   ins_NA = sum(inmove_NA),
                   outs_mover = sum(outmove_mover),
                   out_NA = sum(outmove_NA)), by=c("tcBEID", "year")]
timer <- Sys.time()-timer
timer

#### Extract aggregated panel ----

# aggregate AKM result to tcBEID firm level
tcBEIDs_year <- RINPs_year[ ,.(rhwage=rhwage[1], 
                               rhwage_mover = rhwage_mover[1],
                               rhwage_stayer = rhwage_stayer[1],
                              resid_1=resid_1_mean[1],
                              firm_FE=firm_FE[1], 
                              ins_mover = ins_mover[1],
                              ins_NA = ins_NA[1],
                              outs_mover = outs_mover[1],
                              out_NA = out_NA[1],
                              workerFE_mover = workerFE_mover[1],
                              workerFE_stayer = workerFE_stayer[1],
                              workerFE_moverin=workerFE_moverin[1],
                              workerFE_moverout=workerFE_moverout[1],
                              workerFE_NAin=workerFE_NAin[1],
                              workerFE_NAout=workerFE_NAout[1],
                              workerFE_in=workerFE_in[1],
                              workerFE_out=workerFE_out[1],
                              workerFE=workerFE[1], 
                              workerFE_sd=sd(worker_FE),
                              cont_observed_firm=cont_observed_firm[1],
                              years_panel_firm=years_panel_firm[1],
                              entry_year_firm=entry_year_firm[1],
                              observed_until2018 = observed_2018[1],
                              workers=.N) , by=c("tcBEID", "year")]
gc()


# make a seperate SBEID panel to add firm level data
SBEIDs_year <- RINPs_year[ ,.(tcBEID=tcBEID[1]) , by=c("SBEID", "year")]

# delete extra cols from RINPs_year again and set regular key
RINPs_year[, names(RINPs_year)[(k+1):length(names(RINPs_year))] := NULL]
rm(k)
setkeyv(RINPs_year, c("RINP", "year"))
gc()


#### Add SBEID level variables ----

#### size class, nace ####
nace <- readRDS(paste0(map_data2, "sizeclass_nace.rds"))
## set names
setnames(nace, 1:5, c("SBEID", "year", "quarter", "sizeclass", "nace"))
## change type of SBEID
nace[, SBEID := paste0(SBEID)]
## I only need 2006:2018
nace <- nace[year>2005 & year<2019,]
## aggregate to yearly level (pick one available or if multiple then latest recorded quarter
nace <- nace[, .(sizeclass=sizeclass[which.max(quarter)], nace=nace[which.max(quarter)]), by=c("year", "SBEID")]
nace[, sizeclass := as.integer(sizeclass)]
## merge with SBEIDs_year
SBEIDs_year <- merge(SBEIDs_year, nace, by=c("SBEID", "year"), all.x=TRUE)
rm(nace)
gc()

#### Add nace21 ####
# add 21 Level 1 nace codes in nace21
nace <- as.data.table(read_excel(paste0(map_data2, "SBI_2008_versie_2018_update_2019.xlsx"), sheet="matrix", col_names=TRUE))
nace[, nace2:=paste0(substr(nace,1,2))]
nace[, nace:=NULL]
SBEIDs_year[, nace2 := substr(nace,1,2)]
SBEIDs_year <- merge(SBEIDs_year, nace, by=c("nace2"), all.x=TRUE)
SBEIDs_year[, nace2 := NULL]
rm(nace)

# impute nace21 as most commonly observed nace21
# function to calculate the FIRST mode
first_mode <- function(vec, remove.NA = TRUE, char_type = TRUE){
  if (remove.NA == TRUE) vec <- vec[!is.na(vec)]
  if (length(vec) == 0) {if (char_type==TRUE) return(NA_character_) else return(NA)} # this is to ensure that NA is return if vec is empty
  vec_uniq <- unique(vec) 
  return(vec_uniq[which.max(tabulate(match(vec,vec_uniq)))])
}
SBEIDs_year[, nace21 := first_mode(nace21, remove.NA = TRUE), by=SBEID]
SBEIDs_year[is.na(nace21), nace21 := "MISSING"]


#### itg: international trade in goods ####
itg <- readRDS(paste0(map_data2, "itg.rds"))
## set names
setnames(itg, 2, "year")
## change types
itg[, SBEID := paste0(BEID)]
itg[, BEID := NULL]
itg[, import := as.numeric(import)]
itg[, export := as.numeric(export)]
itg[, reexport := as.numeric(reexport)]
itg[, year := as.integer(year)]

# add tcBEID
itg <- merge(itg, unique(SBEIDs_year[, .(SBEID=SBEID, tcBEID=tcBEID)]), by=c("SBEID"))

## aggregate to tcBEID-year level
itg <- itg[, .(import_countries = sum(import>0, na.rm=TRUE),
               export_countries = sum(export > 0, na.rm=TRUE),
               reexport_countries = sum(reexport > 0, na.rm=TRUE),
               import_value = sum(import/1000, na.rm=TRUE),
               export_value = sum(export/1000, na.rm=TRUE),
               reexport_value = sum(reexport/1000, na.rm=TRUE)), by=c("tcBEID", "year")]

## merge with SBEIDs_year
SBEIDs_year <- merge(SBEIDs_year, itg, by=c("tcBEID", "year"), all.x=TRUE)
rm(itg)

# impute itg
SBEIDs_year[is.na(import_countries), import_countries:=0]
SBEIDs_year[is.na(export_countries), export_countries:=0]
SBEIDs_year[is.na(reexport_countries), reexport_countries:=0]
SBEIDs_year[is.na(import_value), import_value:=0]
SBEIDs_year[is.na(export_value), export_value:=0]
SBEIDs_year[is.na(reexport_value), reexport_value:=0]

gc()

#### ps: Productiestatistik ---
# read in
ps <- readRDS(paste0(map_data2, "sbs.rds"))

# set names
setnames(ps, 2, "year")

# change types
ps[, SBEID := paste0(BEID)]
ps[, BEID := NULL]
ps[, value := as.numeric(value)]

# add tcBEID
ps <- merge(ps, unique(SBEIDs_year[, .(SBEID=SBEID, tcBEID=tcBEID)]), by=c("SBEID"))
ps <- ps[, .(sales_ps = sum(value[which(variable==12110)], na.rm=TRUE),
             profit_ps = sum(value[which(variable==12130)], na.rm=TRUE),
             employees_ps = sum(value[which(variable==111000)], na.rm=TRUE),
             productiewaarde = sum(value[which(variable==12120)], na.rm=TRUE),
             toegevoedgewaarde_factorkosten = sum(value[which(variable==12150)], na.rm=TRUE),
             totaal_aankoop_goederen_diensten = sum(value[which(variable==13110)], na.rm=TRUE),
             aankoop_goederen_wederverkoop = sum(value[which(variable==13120)], na.rm=TRUE),
             kosten = sum(c(value[which(variable==13310)], 
                            value[which(variable==13330)],
                            value[which(variable==13410)],
                            value[which(variable==13411)],
                            value[which(variable==13420)],
                            value[which(variable==13430)]), na.rm = TRUE),
             investeringen = sum(c(value[which(variable==15420)],
                                   value[which(variable==15441)]), na.rm=TRUE)),
         by=c("tcBEID", "year")]
# merge with SBEIDs_year
SBEIDs_year <- merge(SBEIDs_year, ps, by=c("tcBEID", "year"), all.x=TRUE)
rm(ps)
gc()


#### Import UCI and define foreign multinationals ----

# Read in uci
uci <- readRDS(paste0(map_data2, "uci.rds"))
uci[, SBEID:=paste0(BEID)]
uci[, BEID:=NULL]
uci[, year:=as.integer(jaar)]
uci[, jaar:=NULL]
uci[UCI=="", UCI:=NA_character_]
uci[is.na(UCI), UCI:=NA_character_]

# foreign multinationals are firms that have a UCI!=NL
uci[, bui_mul := fifelse(UCI!="NL" & UCI!="nl" & UCI!="NL:" & !is.na(UCI), TRUE, FALSE)]

# merge UCI with SBEIDs_year
SBEIDs_year <- merge(SBEIDs_year, uci, by=c("SBEID", "year"), all.x=TRUE)
SBEIDs_year[is.na(bui_mul), bui_mul := FALSE]
SBEIDs_year[is.na(UCI), UCI:="MISSING"] 
rm(uci)

#### Merge with information on company groups ----
## read in 
beog <- readRDS(paste0(map_data2,"beog.rds"))
beog[, SBEID := paste(BEID)]
beog[, BEID := NULL]
setnames(beog, "jaar", "year")

## merge 
SBEIDs_year <- merge(SBEIDs_year, beog, by = c("year", "SBEID"), all.x = TRUE)
rm(beog)
gc()


#### SFGO INFOMRATION ----
SFGO <- fread(paste0(map_data2,"/SFGO_2006_2015.txt", sep=" "))
SFGO <- SFGO[, .(OG_BEID=as.integer(OND_ID), year=as.integer(JAAR),
                 DEELNAME_BULA_OND=as.integer(DEELNAME_BULA_OND), BULA_DOCHTERS=BULA_DOCHTERS, UCI_SFGO = UCI, UBO=UBO, OZ_LAND=OZ_LAND,
                 Activa_Eind = as.integer(gsub(",", ".",B37)), 
                 mat_vaste_activa_Eind = as.numeric(gsub(",", ".",B03)), immat_vaste_activa_Eind = as.numeric(gsub(",", ".",B01)), 
                 gestort_kapitaal_Eind = as.numeric(gsub(",", ".",B51)))]
SBEIDs_year <- merge(SBEIDs_year, SFGO, by = c("year", "OG_BEID"), all.x = TRUE)
rm(SFGO)


#### Find first MNE year per company group ####

# Per year in a company group, find out if it is a foreign multinational
SBEIDs_year[!is.na(OG_BEID), OG_bui_mul := ifelse(any(bui_mul==TRUE), TRUE, FALSE), by=c("OG_BEID", "year")]

# per OG_BEID find out in which year the first foreign MNE status was observed
find_year <- function(OG_bui_mul, year){
  if (!any(OG_bui_mul==TRUE)) return(NA_integer_)
  # positions of OG_bui_mul==TRUE
  pos <- which(OG_bui_mul==TRUE)
  # minimum year with TRUE
  result <- min(year[pos])
  return(result)
}
SBEIDs_year[!is.na(OG_BEID), year_MNE_OG := find_year(OG_bui_mul, year), by=OG_BEID]

#### aggregate to tcBEID level and merge with tcBEIDs_year ----
min2 <- function(x) {if (length(x[!is.na(x)])>0) min(x, na.rm=TRUE) else NA_integer_}
SBEIDs_year[, year_MNE_OG := min2(year_MNE_OG), by=c("tcBEID", "year")]


#### aggregate OGBEIDs to tcBEID level ####
# How many OGBEIDs per year and tcBEIDs?
SBEIDs_year[, number_OG_BEIDs := uniqueN(OG_BEID, na.rm=TRUE), by=c("tcBEID", "year")]

# How many SBEIDs per OGBEID per year
setkeyv(SBEIDs_year, c("OG_BEID", "year"))
SBEIDs_year[!is.na(OG_BEID), OG_SBEIDs := uniqueN(SBEID), by=c("OG_BEID", "year")]
tt <- SBEIDs_year[!is.na(OG_BEID), c("OG_BEID", "year", "OG_SBEIDs")]
tt <- unique(tt)

# In years where >1 OG_BEID is observed, adjust OG_BEID
setkeyv(SBEIDs_year, c("tcBEID", "year"))

IDs <- SBEIDs_year[number_OG_BEIDs>1, tcBEID]

adjust.OGBEID <- function(number_OG_BEIDs, year, OG_BEID, tcBEID, OG_SBEIDs){
  # make copy of OG_BEIDs
  OG_new <- OG_BEID
  
  # find years of these positions
  pos <- which(number_OG_BEIDs>1)
  if (length(pos)==0) {
    warning(paste("No positions.", tcBEID), "\n"); return(OG_BEID) }
  
  years <- unique(year[pos])
  
  # set OG_BEID to overlapping OG_BEID
  for (k in years){
    # get current OG_BEIDs
    current <- OG_BEID[which(year==k)]
    
    # get next OG_BEIDs
    following <- OG_BEID[which(year==(k+1))]
    
    if (length(following)==0){
      # If no next year, then set to non overlapping of earlier year
      earlier <- OG_BEID[which(year==(k-1))]
      overlap <- current[!(current %in% earlier)]
      
    } else overlap <- current[current %in% following]
    
    # find overlap and assign as OG_BEID
    if (length(overlap)>1) {
      # if there is a bigger overlap, set to OG_BEID to one with largest OG_SBEIDs, if all same, choose first one
      overlap <- current[which.max(OG_SBEIDs[which(year==k)])]
    } 
    
    if (length(overlap)==0) {
      # if there is no overlap, set to OG_BEID to one with largest OG_SBEIDs, if all same, choose first one
      overlap <- current[which.max(OG_SBEIDs[which(year==k)])]
    } 
    
    # assign
    OG_new[which(year==k)] <- overlap
    
  }
  # return result
  return(OG_new)
}

SBEIDs_year[tcBEID %in% IDs, OG_BEID := adjust.OGBEID(number_OG_BEIDs, year, OG_BEID, tcBEID, OG_SBEIDs), by=tcBEID]
rm(IDs)

SBEIDs_year[, OG_SBEIDs := NULL]
# add OG_SBEIDs again
SBEIDs_year <- merge(SBEIDs_year, tt, by=c("OG_BEID", "year"), all.x=TRUE)
rm(tt)
gc()


#### Inflate to 2018 ----

inflation_factor <- readRDS(paste0(map_data, "step1_basics/cpi_year.rds"))

SBEIDs_year <- merge(SBEIDs_year, inflation_factor, by="year")
SBEIDs_year[, import_value := import_value*factor]
SBEIDs_year[, export_value := export_value*factor]
SBEIDs_year[, reexport_value := reexport_value*factor]
SBEIDs_year[, sales_ps := sales_ps*factor]
SBEIDs_year[, profit_ps := profit_ps*factor]
SBEIDs_year[, productiewaarde := productiewaarde*factor]
SBEIDs_year[, toegevoedgewaarde_factorkosten := toegevoedgewaarde_factorkosten*factor]
SBEIDs_year[, totaal_aankoop_goederen_diensten := totaal_aankoop_goederen_diensten*factor]
SBEIDs_year[, aankoop_goederen_wederverkoop := aankoop_goederen_wederverkoop*factor]
SBEIDs_year[, kosten := kosten*factor]
SBEIDs_year[, investeringen := investeringen*factor]
SBEIDs_year[, Activa_Eind := Activa_Eind*factor]
SBEIDs_year[, mat_vaste_activa_Eind := mat_vaste_activa_Eind*factor]
SBEIDs_year[, immat_vaste_activa_Eind := immat_vaste_activa_Eind*factor]
SBEIDs_year[, gestort_kapitaal_Eind := gestort_kapitaal_Eind*factor]
SBEIDs_year[, factor:=NULL]
rm(inflation_factor)
gc()


#### aggregate whole panel ----
SBEIDs_year <- SBEIDs_year[, .(only_fake = ifelse(!any(substr(SBEID,1,1) %in% 1:9), TRUE, FALSE), # need this for identifying MNEs
                               
                               # nace, sizeclass
                               nace = nace[which(!is.na(nace))[1]], 
                               nace21 = nace21[which(!is.na(nace21))[1]],
                               sizeclass = fifelse(length(sizeclass)>0, max(sizeclass, na.rm=TRUE), NA_real_), # have to pick this somehow, maybe rather match on observed employees
                               
                               # ihg
                               import_countries=import_countries[1], 
                               export_countries=export_countries[1],
                               reexport_countries=reexport_countries[1], 
                               import_value=import_value[1], 
                               export_value=export_value[1], 
                               reexport_value=reexport_value[1],
                               
                                # ps
                               sales_ps = sales_ps[1], 
                               profit_ps = profit_ps[1], 
                               employees_ps = employees_ps[1],
                               productiewaarde = productiewaarde[1],
                               toegevoedgewaarde_factorkosten = toegevoedgewaarde_factorkosten[1],
                               totaal_aankoop_goederen_diensten = totaal_aankoop_goederen_diensten[1],
                               aankoop_goederen_wederverkoop = aankoop_goederen_wederverkoop[1],
                               kosten = kosten[1],
                               investeringen = investeringen[1],

                               # SFGO
                               DEELNAME_BULA_OND = paste(DEELNAME_BULA_OND, collapse=";"),
                               BULA_DOCHTERS = paste(BULA_DOCHTERS, collapse=";"),
                               UCI_SFGO = paste(UCI_SFGO, collapse=";"),
                               UBO = paste(UBO, collapse=";"),
                               OZ_LAND = paste(OZ_LAND, collapse=";"),
                               Activa_Eind = sum(Activa_Eind, na.rm=TRUE),
                               mat_vaste_activa_Eind = sum(mat_vaste_activa_Eind, na.rm=TRUE),
                               immat_vaste_activa_Eind = sum(immat_vaste_activa_Eind, na.rm=TRUE),
                               gestort_kapitaal_Eind = sum(gestort_kapitaal_Eind, na.rm=TRUE),

                               ## MNE
                               bui_mul = as.logical(max(bui_mul)),
                               year_MNE_OG = year_MNE_OG[1],
                               OG_BEID = OG_BEID[1],
                               OG_SBEIDs = OG_SBEIDs[1],
                               UCI = paste(UCI, collapse=";")),
                           by=c("tcBEID", "year")]


# merge with tcBEIDs_year
tcBEIDs_year <- merge(tcBEIDs_year, SBEIDs_year, by=c("tcBEID", "year"))
rm(SBEIDs_year)



setorderv(tcBEIDs_year, cols=c("tcBEID", "year"))

#### Add additional variables ----
RINPs_year2 <- readRDS(paste0(map_data,"step5/yeartcBEID_network.rds"))
tcBEID_SBEID <- RINPs_year2[, .(tcBEID=tcBEID[1]), by=SBEID]
RINPs_year2[, `:=` (female_workers = sum(female==TRUE),
                    mean_age = mean(age),
                    max_age = max(age),
                    min_age = min(age),
                    extra_payments = mean(exp(lrhwage)-rhwage_basic),
                    mean_lrhwage = mean(lrhwage)) , by=c("tcBEID", "year")]
RINPs_year2 <- RINPs_year2[, .(female_workers=female_workers[1],
                               mean_age=mean_age[1],
                               max_age = max_age[1],
                               min_age = min_age[1],
                               extra_payments = extra_payments[1],
                               mean_lrhwage = mean_lrhwage[1]), by=c("tcBEID", "year")]
RINPs_year2[, year := as.integer(paste0(year))]
tcBEIDs_year <- merge(tcBEIDs_year, RINPs_year2, by=c("tcBEID", "year"), all.x=TRUE)
rm(RINPs_year2)
gc()

# age of the firm
birthyear_firm <- readRDS(paste0(map_data2,"birthyear_firm.rds"))
birthyear_firm <- merge(birthyear_firm, tcBEID_SBEID, by="SBEID")
birthyear_firm <- birthyear_firm[,.(birthday_firm=min(birthday_firm, na.rm=TRUE)), by=tcBEID]
tcBEIDs_year <- merge(tcBEIDs_year, birthyear_firm, by=c("tcBEID"), all.x=TRUE)
rm(birthyear_firm)
tcBEIDs_year[, birthday_firm := as.integer(birthday_firm)]
# fix for linked fake tcBEIDs and missing birthdays
setorderv(tcBEIDs_year, c("tcBEID", "year"))
tcBEIDs_year[, birthday_firm := min(c(birthday_firm[1], year[1]), na.rm=TRUE), by=tcBEID]
tcBEIDs_year[, age_firm := year-birthday_firm]
rm(tcBEID_SBEID)

#### PC6 / total employees #####
postcode_employees <- readRDS(paste0(map_data2,'postcode_employees_regiobase.rds'))
postcode_employees[, year := as.integer(paste0(year))]
postcode_employees[, SBEID := paste0(BEID)]
postcode_employees[, BEID := NULL]
postcode_employees <- postcode_employees[year<=2018,]

# per year, SBEID keep PC6 where most employees are
postcode_employees <- postcode_employees[, .(PC6_main = PC6[which.max(employees_regiobase)],
                                             employees_total=sum(employees_regiobase)), by=c("SBEID", "year")]

# merge with tcBEID, SBEID translation table
trans_SBEID_tcBEID <- readRDS(paste0(map_data,"step2/trans_SBEID_tcBEID.rds"))



postcode_employees <- merge(postcode_employees, trans_SBEID_tcBEID, by=c("SBEID", "year"), all.x=TRUE)
rm(trans_SBEID_tcBEID)
# assign tcBEID for missing tcBEIDs
postcode_employees[is.na(tcBEID), tcBEID := SBEID]

# per year, tcBEID keep PC6 where most employees are
postcode_employees <- postcode_employees[, .(PC6_main = PC6_main[which.max(employees_total)],
                                             employees_total=sum(employees_total)), by=c("tcBEID", "year")]

# merge with tcBEIDs_year
tcBEIDs_year <- merge(tcBEIDs_year, postcode_employees, by=c("tcBEID", "year"), all.x=TRUE)
rm(postcode_employees)


#### Impute nace21 and nace again (tcBEID level) ----
# function to calculate the FIRST mode
first_mode_nace <- function(vec, remove = "MISSING", char_type = TRUE){
  if (length(remove)!=0) vec <- vec[!(vec==remove)]
  if (length(vec) == 0) {if (char_type==TRUE) return(NA_character_) else return(NA)} # this is to ensure that NA is return if vec is empty
  vec_uniq <- unique(vec) 
  return(vec_uniq[which.max(tabulate(match(vec,vec_uniq)))])
}
setkeyv(tcBEIDs_year, cols=c("tcBEID", "year"))
tcBEIDs_year[is.na(nace21), nace21 := "MISSING"]
tcBEIDs_year[, nace21 := first_mode_nace(nace21, remove = "MISSING"), by=tcBEID]
tcBEIDs_year[is.na(nace21), nace21 := "MISSING"]

tcBEIDs_year[is.na(nace), nace := "MISSING"]
tcBEIDs_year[, nace := first_mode_nace(nace, remove = "MISSING"), by=tcBEID]
tcBEIDs_year[is.na(nace), nace := "MISSING"]

# revert nace and nace21 to NA
tcBEIDs_year[nace21=="MISSING", nace21:=NA]
tcBEIDs_year[nace=="MISSING", nace:=NA]

# create nace-2-digit indicator
tcBEIDs_year[, nace_2digit:=paste0(substr(nace,1,2))]
# add nace21 again here, just to be on the safe side.
nace <- as.data.table(read_excel(paste0(map_data2, "SBI_2008_versie_2018_update_2019.xlsx"), sheet="matrix", col_names=TRUE))
nace[, nace_2digit:=paste0(substr(nace,1,2))]
nace[, nace:=NULL]
tcBEIDs_year[, nace21 := NULL]
tcBEIDs_year <- merge(tcBEIDs_year, nace, by=c("nace_2digit"), all.x=TRUE)
rm(nace)
gc()

#### Reidentify treatment group ---- 
# figure out if a firm has a foreign daughter (from DANFLEX) in current year
tcBEIDs_year[, BULA_DOCHTERS := (grepl("J", BULA_DOCHTERS, fixed=TRUE))]

# remove firms that were ever Dutch MNE's from the treatment group 

#### Identify foreign takeovers ----
brownfield_tcBEID <- function(year, BULA_DOCHTERS, bui_mul, brownfield_bui, employees, ins){
  n <- length(year)
  
  # if already identified, there's nothing to do
  if (any(brownfield_bui==TRUE)) return(rep(FALSE, n))
  
  # if never a MNE, there's nothing to do
  if (all(bui_mul==FALSE)) return(rep(FALSE, n))
  
  # if begins as MNE, there's nothing to do
  if (bui_mul[1]==TRUE) return(rep(FALSE, n))
  
  # if not continuously observed, don't identify as investment
  if (!all(diff(year)==1)) return(rep(FALSE, n))
  
  # find first MNE position
  pos <- which(bui_mul==TRUE)[1]
  
  # if ever a DUTCH MNE before the acquistion, there's nothing to do
  if (any(BULA_DOCHTERS[1:(pos-1)])==TRUE) return(rep(FALSE, n))
  
  # Check if MNE for at least 4 periods
  # if not, don't identify as investment
  if (length(bui_mul[pos:length(bui_mul)])<4) return(rep(FALSE, n))
  
  # Check if not MNE for at least 3 periods
  if (length(bui_mul[1:(pos-1)])<3) return(rep(FALSE, n))
  
  # if not at least 5 employees in 3 years before and after takeover, there is nothing to do
  if (!all(employees[(pos-3):(pos+3)]>=5)) return(rep(FALSE, n))
  
  # check if MNE over whole period to end
  # if not, don't identify as investment
  if (!all(bui_mul[pos:length(bui_mul)]==TRUE)) return(rep(FALSE, n))
  
  # check if new employees actually enter the firm
  if (!sum(ins[pos:length(ins)]>1)) return(rep(FALSE, n))
  
  # now we have found brownfield investments
  brownfield_bui <- rep(FALSE, length(year))
  brownfield_bui[pos:length(year)] <- TRUE
  
  return(list(brownfield_bui)) 
}
# Identify Brownfield investments
setorderv(tcBEIDs_year, c("tcBEID", "year"))
tcBEIDs_year[, brownfield_bui:=FALSE]
tcBEIDs_year[, c("brownfield_bui") := brownfield_tcBEID(year, BULA_DOCHTERS, bui_mul, brownfield_bui, workers, ins=ins_mover+ins_NA), by=tcBEID]

# Adjust treatment vars again
tcBEIDs_year[, c("treatment_group", "time_takeover", "takeover_year", "pre_knowledge") := NULL]
tcBEIDs_year[, treatment_group:=fifelse(any(brownfield_bui==TRUE), TRUE, FALSE), by=tcBEID]
tcBEIDs_year[treatment_group==TRUE, time_takeover := year-year[which(brownfield_bui==TRUE)[1]], by=tcBEID]
tcBEIDs_year[treatment_group==TRUE, takeover_year := year[which(time_takeover==0)], by=tcBEID]
tcBEIDs_year[treatment_group==TRUE, pre_knowledge := fifelse(year_MNE_OG[year==takeover_year] < takeover_year, TRUE, FALSE), by=tcBEID]


#### identify foreign acquistions ----
foreign_acq_tcBEID <- function(year, bui_mul, BULA_DOCHTERS){
  n <- length(year)
  # if never a foreign MNE, there's nothing to do
  if (all(bui_mul==FALSE)) return(rep(FALSE, n))
  
  # if begins as MNE, there's nothing to do
  if (bui_mul[1]==TRUE) return(rep(FALSE, n))
  
  # if not continuously observed, don't identify as investment
  if (!all(diff(year)==1)) return(rep(FALSE, n))
  
  # find first MNE position (that is the first the firm changes from Dutch to Foreign)
  pos <- which(bui_mul==TRUE)[1]
  
  # if ever a DUTCH MNE before the acquistion, there's nothing to do
  if (any(BULA_DOCHTERS[1:(pos-1)])==TRUE) return(rep(FALSE, n))
  
  # Check if not MNE for at least 3 periods
  if (length(bui_mul[1:(pos-1)])<2) return(rep(FALSE, n))
  
  # now we have found a foreign acquisition
  res <- rep(FALSE, length(year))
  res[(pos-1)] <- TRUE
  
  return(res) 
}
# Identify Brownfield investments
setorderv(tcBEIDs_year, c("tcBEID", "year"))
tcBEIDs_year[, foreign_acq.lead :=FALSE]
tcBEIDs_year[, c("foreign_acq.lead") := foreign_acq_tcBEID(year, bui_mul, BULA_DOCHTERS), by=tcBEID]


#### Prepare potential control and treatment group ----

# figure out treatment firms with NA before takeover
tcBEIDs_year[treatment_group==TRUE, NA_before := fifelse(grepl("MISSING", UCI[which(year==(takeover_year-1))], fixed = TRUE), TRUE,FALSE), by=tcBEID]

# for each year find out how long a firm has lived and will live on
tcBEIDs_year[, active_since := year-year[1], by=tcBEID]
tcBEIDs_year[, active_until := year[length(year)]-year, by=tcBEID]

# select potential control firms on:
# continuously observed for 7 years
# never a foreign multinational
# not a dutch multinational
# same nace_2digit
# at least 5 workers over potential takeover period
nace_treatment <- tcBEIDs_year[treatment_group==TRUE & !is.na(nace_2digit), unique(nace_2digit)]
IDs <- tcBEIDs_year[, (all(diff(year)==1) & all(sum(diff(year))>=6) & all(bui_mul==FALSE) & all(BULA_DOCHTERS==FALSE) & any(nace_2digit %in% nace_treatment)), by=tcBEID][V1==TRUE, tcBEID]
rm(nace_treatment)

# for these firm IDs create potential control dummy at potential control year
tcBEIDs_year[, potential_control := FALSE]
tcBEIDs_year[((tcBEID %in% IDs) & (active_since>=2) & (active_until>=4)), potential_control := TRUE]
#rm(IDs)


# keep these points if at least 5 workers in current, past two and next 4 years
rolling_all <- function(potential_control, workers){
  # find potential control points
  idx <- which(potential_control==TRUE)
  
  # make matrix of lagged and leaded worker numbers
  mat <- cbind(workers[idx], workers[(idx-1)], workers[(idx-2)], workers[(idx+1)], workers[(idx+2)], workers[(idx+3)], workers[(idx+4)])
  
  # check if workers above 5 in each row
  cond <- apply(mat, 1, function(x) all(x>=5))
  
  # adjust potential control vectors
  potential_control[idx] <- cond
  
  # return result
  return(potential_control)
}
tcBEIDs_year[tcBEID %in% IDs, potential_control := rolling_all(potential_control, workers), by=tcBEID]
rm(IDs)

#### Prepare other variables ----

# as a robustness check want to match at time_to_takeover==-2 instead of time_to_takeover==-1
# so lead foreign_acq.lead again
tcBEIDs_year[, foreign_acq.lead2 := shift(foreign_acq.lead, n=1, type=c("lead")), by=tcBEID]

# calculate some vars 
tcBEIDs_year[, log_wage := log(rhwage)]
tcBEIDs_year[, log_employment := log(workers)]
# firm/Worker FEs
tcBEIDs_year[, workerFE_2 := workerFE^2]
tcBEIDs_year[, firm_FE_2 := firm_FE^2] 

# variance of workerFEs
tcBEIDs_year[, workerFE_var := workerFE_sd^2]

# keep some lags and leads of vars of interest to fine-tune matching quality
tcBEIDs_year[, c("workerFE.lag1", "workerFE.lag2") := shift(workerFE, n=c(1,2), type="lag"), by=tcBEID]
tcBEIDs_year[, c("firm_FE.lag1", "firm_FE.lag2") := shift(firm_FE, n=c(1,2), type="lag"), by=tcBEID]
tcBEIDs_year[, c("log_wage.lag1", "log_wage.lag2") := shift(log_wage, n=c(1,2), type="lag"), by=tcBEID]
tcBEIDs_year[, c("mean_lrhwage.lag1", "mean_lrhwage.lag2") := shift(mean_lrhwage, n=c(1,2), type="lag"), by=tcBEID]
tcBEIDs_year[, c("log_employment.lag1", "log_employment.lag2") := shift(log_employment, n=c(1,2), type="lag"), by=tcBEID]
tcBEIDs_year[, c("workerFE_var.lag1", "workerFE_var.lag2") := shift(workerFE_var, n=c(1,2), type="lag"), by=tcBEID]


# growth rates of workerFE and firm_FE
tcBEIDs_year[, firm_FE.change1 := (firm_FE-firm_FE.lag1)]
tcBEIDs_year[, workerFE.change1 := (workerFE-workerFE.lag1)]
tcBEIDs_year[, firm_FE.change2 := (firm_FE.lag1-firm_FE.lag2)]
tcBEIDs_year[, workerFE.change2 := (workerFE.lag1-workerFE.lag2)]
tcBEIDs_year[, log_wage.change := log_wage-log_wage.lag1]
tcBEIDs_year[, log_wage.change2 := log_wage.lag1-log_wage.lag2]
tcBEIDs_year[, mean_lrhwage.change := mean_lrhwage-mean_lrhwage.lag1]
tcBEIDs_year[, mean_lrhwage.change2 := mean_lrhwage.lag1-mean_lrhwage.lag2]
tcBEIDs_year[, log_employment.change := log_employment-log_employment.lag1]
tcBEIDs_year[, log_employment.change2 := log_employment.lag1-log_employment.lag2]

tcBEIDs_year[, mean_lrhwage.2change := mean_lrhwage-mean_lrhwage.lag2]
tcBEIDs_year[, log_wage.2change := log_wage-log_wage.lag2]
tcBEIDs_year[, log_employment.2change := log_employment-log_employment.lag2]
tcBEIDs_year[, firm_FE.2change := firm_FE-firm_FE.lag2]
tcBEIDs_year[, workerFE.2change := workerFE-workerFE.lag2]



# prepare additional PSM vars for all firms
# exports, imports, reexports
#tcBEIDs_year[sales_ps!=0, export_sales := export_value/(abs(sales_ps))]
tcBEIDs_year[, log_exports := log(export_value+1)]
tcBEIDs_year[, log_imports := log(import_value+1)]
tcBEIDs_year[, log_reexports := log(reexport_value+1)]

# dummies for export, import, reexport
tcBEIDs_year[, import := import_countries>0]
tcBEIDs_year[, export := export_countries>0]
tcBEIDs_year[, reexport := reexport_countries>0] 

# number of export, import, reexport countries
tcBEIDs_year[, log_importcountries := log(import_countries+1)]
tcBEIDs_year[, log_exportcountries := log(export_countries+1)]
tcBEIDs_year[, log_reexportcountries := log(reexport_countries+1)]


# firm age
tcBEIDs_year[, log_firmage := log(age_firm)]
tcBEIDs_year[log_firmage==-Inf, log_firmage := NA]

# log sales
tcBEIDs_year[sales_ps!=0, log_sales := log(abs(sales_ps))]
tcBEIDs_year[, log_sales_worker := log_sales/workers]

# change in log sales
tcBEIDs_year[, log_sales.change := shift(log_sales, n=1, type="lag"), by=tcBEID]
tcBEIDs_year[, log_sales.change := log_sales-log_sales.change]

# labor productivity (log sales/ log employment)
tcBEIDs_year[log_employment!=0, log_laborproductivity := log_sales/log_employment]


# age, share females
tcBEIDs_year[, log_meanage := log(mean_age)]
tcBEIDs_year[, share_females := female_workers/workers]


#### Save result ----
saveRDS(tcBEIDs_year, paste0(map_output_here,"tcBEIDs_year_matching.rds"))

