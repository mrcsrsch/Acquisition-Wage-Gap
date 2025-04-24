##################################################################################
# Read and prepare GBA/NIETGBA
##################################################################################
#### packages ######
if (!require("dplyr")) install.packages("dplyr"); library("dplyr") # anti_join

#### output dirs #####
if (!dir.exists(paste0(map_data, "step1_basics/"))) dir.create(paste0(map_data, "step1_basics/"))
map_output_here <- paste0(map_data, "step1_basics/")
##################################################################################
##################################################################################

# Read in gba and niet-gba (currently 2018) ----
# Note: need to clean this slightly and then merge. If in both, prefer GBA. 
## load gba & manipulate 
gba <- fread(paste0(map_data2,"GBAPERSOON/2018/DATA/GBAPERSOON2018.csv"), sep=";",
             colClasses = c("character",
                            "character",
                            "integer",
                            "integer",
                            "character",
                            "NULL",
                            "integer",
                            "NULL",
                            "NULL"))

### manipulate ====
setnames(gba, 1, "RINPERSOONS")
gba[,female:= 1*(GBAGESLACHT == 2)]
gba[,gebdat := ymd(paste0(GBAGEBOORTEJAAR,GBAGEBOORTEMAAND,"01"))]
keep_cols <- c("RINPERSOONS", "RINPERSOON", "female", "gebdat", "GBAGEBOORTELAND")
gba <- gba[, .SD, .SDcols=keep_cols]
gba[,"gba" := 1]

## load niet gba & manipulate ----
nietgba <- fread(paste0(map_data2,"NIETGBA/2018/DATA/NIET_GBAPERSOON.csv"), sep=";", 
                 na.strings=c("-" , "--", "---", "----", "NA"),
                 colClasses = c("character",
                                "character",
                                "NULL",
                                "NULL",
                                "NULL",
                                "NULL",
                                "integer",
                                "character",
                                "NULL",
                                "NULL",
                                "character",
                                "integer", 
                                "NULL"
                                ))

## some people are in the gba and nietgba, e.g. by having a job in NL and subsequently moving to NL
### identify and delete those in nietgba
nietgba <- anti_join(nietgba, gba, by=c("RINPERSOONS","RINPERSOON"))
nietgba <- as.data.table(nietgba)

### manipulate =====
nietgba[,female:= 1*(NIETGBAGESLACHT == 2)]
nietgba[,gebdat := ymd(paste0(NIETGBAGEBJAAR,NIETGBAGEBMND,"01"))]
setnames(nietgba, 4, "GBAGEBOORTELAND")
keep_cols <- c("RINPERSOONS", "RINPERSOON", "female", "gebdat", "GBAGEBOORTELAND")
nietgba <- nietgba[, .SD, .SDcols=keep_cols]
nietgba[,"gba" := 0]


## Append: IDs in gba and nietgba are unique now ----
gba <- rbind(gba, nietgba)
rm(nietgba)

### create RINP ====
gba[, RINP := paste0(RINPERSOONS, RINPERSOON)]
gba[, c("RINPERSOONS", "RINPERSOON") := NULL]
gc() 

# SAVE, create dir if necessary ----          

saveRDS(gba, file=paste0(map_output_here, "gba.rds"))                  