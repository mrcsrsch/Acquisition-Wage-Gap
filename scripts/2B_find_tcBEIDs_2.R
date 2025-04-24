##################################################################################
# linkedBEID / Identify takeovers
# In this script I implement company ID links through employee flows, following Benedetto et al. 2007
# I identify a time-consistent BEID (tcBEID) for ID changes and 
# linkedBEIDs for the other categories in Benedetto et al 2007.
# Note: The focus is on identifying the tcBEID.
# I start off with the monthly selected SPOLIS panel, I aggregate to the yearly level to finish identifying tcBEIDs.
##################################################################################
#### output dirs #####
if (!dir.exists(paste0(map_data, "step2/"))) dir.create(paste0(map_data, "step2/"))
map_output_here <- paste0(map_data, "step2/")	  
##################################################################################
##################################################################################

#### read in POLIS ----
# Read in non parallel
FSPOLIS <- list()

i <- 0
for (current_year in 2006:2018){
  # message
  cat(paste(Sys.time(), "starting with", current_year, "\n"))
  
  i <- i+1
  # SPOLIS
  path <- paste0(map_data, "step2/fin_SPOLIS/fin_SPOLIS_", current_year,".rds")
  FSPOLIS[[i]]  <- readRDS(path)
  rm(path)

  # only keep necessary cols
  FSPOLIS[[i]] <- FSPOLIS[[i]][, c("RINP", "SBEID", "AANVSSBBUS"), with=FALSE]
  gc()
  
  # message
  cat(paste(Sys.time(), "done with", current_year, "\n"))
}

FSPOLIS <- rbindlist(FSPOLIS)
gc()

#### Calculate flows ----

# lag and lead SBEID, control for gaps in the data
# controlling for gaps is time costly, but assures that we have month-to-month flows

setkey(FSPOLIS, AANVSSBBUS, RINP)
gc()
FSPOLIS[, SBEID.lag := shift_gap(SBEID, AANVSSBBUS, n=1, tolerance=1, fill=NA_character_, type="lag"), by=RINP]
gc()
FSPOLIS[, SBEID.lead := shift_gap(SBEID, AANVSSBBUS, n=1, tolerance=1, fill=NA_character_, type="lead"), by=RINP]
gc()

# aggregate to AANVSSBBUS-SBEID panel and calculate flows

SBEIDs <- FSPOLIS[, .(employees = .N,
                      mode.SBEID.lag = first_mode(SBEID.lag, remove.NA = TRUE),
                      mode.SBEID.lag.freq = N_first_mode(SBEID.lag, remove.NA = TRUE),
                      mode.SBEID.lead =  first_mode(SBEID.lead, remove.NA = TRUE),
                      mode.SBEID.lead.freq = N_first_mode(SBEID.lead, remove.NA = TRUE)), 
                  by=c("AANVSSBBUS", "SBEID")] 
rm(FSPOLIS)
gc()

# calculate ratios
SBEIDs[, mode.SBEID.lag.ratio := mode.SBEID.lag.freq/employees]
SBEIDs[, mode.SBEID.lead.ratio := mode.SBEID.lead.freq/employees]

# order 
setkey(SBEIDs, AANVSSBBUS, SBEID)

# there is an empty SBEID in year == 2008 (already in the source csv)
# --> remove
SBEIDs <- SBEIDs[!SBEID=="", ]

#### Conditions on monthly level ----
# shift employment 7 months in the future
cols_pos <- paste0("employment_", 1:7)
SBEIDs[, (cols_pos) := shift(employees, 1:7, fill=0, type=c("lead")), by=SBEID]
# shift employment 7 months in the past
cols_neg <- paste0("employment_min", 1:7)
SBEIDs[, (cols_neg) := shift(employees, 1:7, fill=0, type=c("lag")), by=SBEID]

# find last observation of SBEID
SBEIDs[, last_obs := AANVSSBBUS[length(AANVSSBBUS)], by=SBEID]
# find first obsersaiton of SBEID
SBEIDs[, first_obs := AANVSSBBUS[1], by=SBEID]


# clean up
gc()

# Identify potential successors
# take those observations where:
# - mode of lagged SBEID is different from SBEID
# - inflow from this mode is >50% of employees in inflow period
# - there are at least 5 employees

## row numbers of potential successors
rowNR_suc <- SBEIDs[employees >=5 & mode.SBEID.lag.ratio >= 0.5 & mode.SBEID.lag!=SBEID, which=TRUE]

# Identify potential predecessors
## extract potential predecessors SBEIDs
input <- SBEIDs[rowNR_suc, .(AANVSSBBUS, mode.SBEID.lag)]
## go back one month
input[, AANVSSBBUS := AANVSSBBUS-months(1)]
## adjust names for merge
setnames(input, c("mode.SBEID.lag"), c("SBEID"))
## row numbers of potential predecessor firms
rowNR_pre <- SBEIDs[input, which=TRUE]

# clean up 
rm(input)
gc()

# identify predecessor and successor relations
## condition W0: mode lead of pot. predecessor is pot. successor
## This is my alpha condition, aka for now I always want this to be true. 
condW0 <- SBEIDs[rowNR_pre, mode.SBEID.lead]==SBEIDs[rowNR_suc, SBEID]
## Therefore, it is handy to subset here
rowNR_suc <- rowNR_suc[condW0]
rowNR_pre <- rowNR_pre[condW0]
rm(condW0)

## condition W1:  predecessor has at least 5 employees, outflow ratio to successor is >=0.8
condW1 <- SBEIDs[rowNR_pre, employees >= 5 & mode.SBEID.lead.ratio >= 0.8]

## condition W2: 80% or more of successors employees come from predecessor 
condW2 <- SBEIDs[rowNR_suc, mode.SBEID.lag.ratio >=0.8]

# clean up
gc()

# Now check drop out conditions
## F1: The predecessor exits
## condition F1: 1. the predecessor firm's employment drops <5 in the 6 month after the transition
## condition F1: 2. the average employment at the predecessor in those 6 month is <10% of employment before
##               3. the predecessor drops out of the panel within the next 6 months after transition
condF1 <- SBEIDs[rowNR_pre, employment_1 < 5 & employment_2 < 5 &
                   employment_3 < 5 & employment_4 < 5 & 
                   employment_5 < 5 & employment_6 <5 & 
                   ((employment_1+employment_2+employment_3+employment_4+employment_5+employment_6)/(6*employees)) < 0.1 &
                as.numeric(difftime(last_obs, AANVSSBBUS))/30<7]

## F2: The successor is an entrant
## condition F2: 1. the successor employment is <5 in 6 months before transition
## condition F2: 2. the average employment as successor is <10% of employment after transition
##               3. the successor enters the panel within 6 months before transition
condF2 <- SBEIDs[rowNR_suc, employment_min1 < 5 & employment_min2 < 5 &
                   employment_min3 < 5 & employment_min4 < 5 & 
                   employment_min5 < 5 & employment_min6 < 5 &
                   ((employment_min1+employment_min2+employment_min3+employment_min4+employment_min5+employment_min6)/(6*employees)) < 0.1 &
                   as.numeric(difftime(AANVSSBBUS, first_obs))/30<7]

# clean up
SBEIDs[, (cols_pos) := NULL]
SBEIDs[, (cols_neg) := NULL]
gc()


#### Identify linked BEIDs ----

# Identify entry and exit periods 
SBEIDs[, entrant := FALSE]
SBEIDs[rowNR_suc, entrant := condF2]
SBEIDs[, exits := FALSE]
SBEIDs[rowNR_pre, exits := condF1]

# Go through cases
# Note that I focus on cases where the successor is an entrant (always require condition F2)
SBEIDs[, linked_BEID := NA_character_]
## ID change (possible foreign acquisition)
SBEIDs[, ID_change := FALSE]
SBEIDs[rowNR_suc, ID_change := (condW1 & condW2 & condF2)]
SBEIDs[rowNR_suc, linked_BEID := ifelse(ID_change, mode.SBEID.lag, NA_character_)]

## Acquisition/merger (with condF2 this is ID change with 80%>X>=50%, X is % in successor)
SBEIDs[, acq_merger := FALSE]
SBEIDs[rowNR_suc, acq_merger := (condW1 & !condW2 & condF2)]
SBEIDs[rowNR_suc, linked_BEID := ifelse(acq_merger, mode.SBEID.lag, linked_BEID)]

## Spinoff/breakout (with condF2 this is a pot. acquisition of a breakout)
SBEIDs[, spinoff_breakout := FALSE]
SBEIDs[rowNR_suc, spinoff_breakout := (!condW1 & condW2 & condF2)]
SBEIDs[rowNR_suc, linked_BEID := ifelse(spinoff_breakout, mode.SBEID.lag, linked_BEID)]

## Reason unclear
SBEIDs[, unclear := FALSE]
SBEIDs[rowNR_suc, unclear := (!condW1 & !condW2 & condF2)]
SBEIDs[rowNR_suc, linked_BEID := ifelse(unclear, mode.SBEID.lag, linked_BEID)]

## clean up vars
SBEIDs[, c("mode.SBEID.lag",
		"mode.SBEID.lag.freq",
		"mode.SBEID.lead",
		"mode.SBEID.lead.freq",
		"mode.SBEID.lag.ratio",
		"mode.SBEID.lead.ratio",
		"last_obs", 
		"first_obs",
		"entrant",
		"exits",
		"employees") := NULL]
rm(condW1, condW2, condF1, condF2)
gc()
			
#### Aggregate to yearly panel ----
SBEIDs[, year := year(AANVSSBBUS)]

# aggregate to yearly level
SBEIDs_year <- SBEIDs[, .(linked_BEID = linked_BEID[which(!is.na(linked_BEID))],
                          ID_change = as.logical(sum(ID_change)),
                          acq_merger = as.logical(sum(acq_merger)),
                          spinoff_breakout = as.logical(sum(spinoff_breakout)),
                          unclear = as.logical(sum(unclear))),
                      by=c("year", "SBEID")]

# clean up 
rm(SBEIDs)
gc()

#### Identify tcBEID ----

# back track linkedBEIDs for ID changes 
# and assign a time consistent BEID for every ID change position (first SBEID in row)
tcBEID.backtrack <- function(BEID, linkedBEID){
  repeat{
    # find position of tcBEID in SBEID vector, if it exists
    matched <- match(linkedBEID, BEID) 
    
    # check exit condition: if there are no matches anymore, I am done.
    a <- sum(is.na(matched))
    b <- length(matched)
    cat(paste(a, b, a==b), "\n")
    if (a==b) break
    
    # update tcBEID vector with reordered tcBEID
    linkedBEID <- ifelse(is.na(linkedBEID[matched]), linkedBEID, linkedBEID[matched]) 
  }
  return(linkedBEID)
}
SBEIDs_year[ID_change==TRUE, tcBEID := tcBEID.backtrack(SBEID, linked_BEID)]

# Assign tcBEID at first position in row
SBEIDs_year <- merge(SBEIDs_year, 
                      unique(SBEIDs_year[!is.na(tcBEID), .(SBEID=tcBEID, tcBEID_temp=tcBEID)]),
                     by=c("SBEID"), all.x = TRUE)
SBEIDs_year[, tcBEID := ifelse(is.na(tcBEID), tcBEID_temp, tcBEID)]
SBEIDs_year[, tcBEID_temp := NULL]

# Impute tcBEID for all observations of a SBEID as tcSBEID
# assign SBEID as tcBEID where there is no tcBEID
SBEIDs_year[, tcBEID := paste0("tc",tcBEID[which(!is.na(tcBEID))[1]]), by=SBEID]
SBEIDs_year[, tcBEID := ifelse(tcBEID=="tcNA", SBEID, tcBEID)]
gc()


# Save translation table
saveRDS(SBEIDs_year[, .(year=year, SBEID=SBEID, tcBEID=tcBEID)], paste0(map_output_here, "trans_SBEID_tcBEID.rds"), compress=FALSE)
rm(SBEIDs_year)
gc()