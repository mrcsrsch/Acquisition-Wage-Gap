############################################################################
# Collection of functions for the project "Do workers or firms drive the foreign acquisition wage premium?"
# Some functions are also in the individual scripts
############################################################################

# ifelse preserving object class
# This is handy for working with dates in ifelse statements
ifelse.preserve <- function(cond, yes, no) structure(ifelse(cond,yes,no), class=class(yes))

# parallel fread: a toy function that allows to read in large files on multiple clusters
fread_par <- function(cl, file.path, header = TRUE, ...){
  require(parallel)
  require(foreach)
  
  # handle fread arguments
  dots <- list(...)
  freadargs <- dots[intersect(names(formals(fread)), names(dots))]
  
  # determine number of rows in file
  file.path <- normalizePath(file.path)
  file.name <- sub(".*\\\\", '', file.path)
  file.path2 <- sub("(.*\\\\)(.*$)", '\\1', file.path)
  cmd1 <- paste("pushd", file.path2)
  cmd2 <- paste("find", '/c /v ""', file.name)
  t <- shell(paste(cmd1, "&&", cmd2), intern=T)[2]
  nrows <- as.numeric(gsub(".*\\: ", "", t))
  
  # split them among the available cores
  ncores <- length(cl)
  points <- ceiling(seq(ifelse(header,1,0), nrows, length.out=ncores+1))
  skips <- (points-1)[1:ncores]
  points <- cbind(skips, (points[-1]-skips)-1)
  rm(points)

  # load information in clusters 
  clusterExport(cl, c("file.path", "freadargs", "points"), envir=environment())
  
  # run parallel
  result <- foreach (x=1:nrow(points), .combine=function(...) rbindlist(list(...)), .multicombine = T, 
                     .packages="data.table") %dopar% {
    do.call(fread, append(list(input = file.path, skip = points[x,1], nrows = points[x,2]), freadargs))
  }
  return(result)
}

# This function collapses adjoining SECM spells.
# It checks for repeated entry of the same SECM category
# then it checks whether the begin and end dates are overlapping
# if this is true, the end date of the last spell is added to the first entry
# and the end date of spells to be deleted is set to "3030-03-03"
# assumes that dates come in the lubridate format 
# assumes data to be ordered by begin date
#
# example call: SECM[, end2 := collapse(SECM, AANVSECM, EINDSECM), by=RINP]
#               SECM <- SECM[end2!=ymd("3030-03-03"),]
collapse <- function(SECM, begin, end){
  require(lubridate)
  l <- rle(SECM)$lengths # length of adjoining duplicates
  # only need to do this if there are actually adjoining duplicates
  if (max(l)>1){
    pos2 <- cumsum(l) # position of last duplicate entry
    pos1 <- pos2 - l + 1 # position of first duplicate entry
    
    # loop through duplicates to compare start and end dates
    ## convert dates to integer: this is much faster 
    begin <- as.integer(begin)
    end <- as.integer(end)
    
    # the idea is to converge pos2 to the first position in steps 
    # pos3 is always the lagged pos2, s.t. at each point two consecutive oberservation are compared
    # and dates are updated on the way if beginning and end match. 
    sum_pos1 <- sum(pos1) # loop constraint (pos1 is always the smallest)
    pos3 <- pos2
    repeat{ 
      if (sum(pos2) == sum_pos1) break # stop if positions are equal
      pos2 <- ifelse(pos2==pos1, pos2, pos2-1) # update position 2
      # update dates if end matches begin (dates are adjoining)
      end[pos2] <- ifelse(end[pos2]+1==begin[pos3], end[pos3], end[pos2]) 
      pos3 <- pos2 # update position 3
    }
    end <- ifelse(duplicated(end), 387218, end) # 387218 is "3030-03-03"
    end <- as_date(end) # convert back to date
  }
  return(end)
}

# this function shifts the vector x according to the time vector time_vec by n rows
# in assessing the time gap between rows it takes a tolerance in MONTHS into account
# fill refers to the value that is put in rows without a shift value 
# type is ONE of c("lag","lead") and n is A SCALAR, it cannot handle simultaneous calls
# to extent it to simultaneous calls need to return a list.   
# example call: SPOLIS[, BEID.lag := shift_gap(BEID, dates, n=1, tolerance=1, fill=NA, type="lag"), by=RINP]
# shift_gap <- function(x, time_vec, n=1, tolerance=1, fill, type="lag"){
#   require(lubridate)
#   if (n > tolerance) stop("Cannot shift over more time than tolerable gap.")
#   return(ifelse(abs(interval(shift(time_vec, n=n, fill=fill, type=type), time_vec) %/% months(1)) > tolerance,fill,
#          shift(x, n=n, fill=fill, type=type)))
# }

# shift gap with integer instead of date: can't use >=31 months
shift_gap <- function(x, time_vec, n=1, tolerance=1, fill, type="lag"){
  require(lubridate)
  if (n > tolerance) stop("Cannot shift over more time than tolerable gap.")
  time_vec <- as.numeric(time_vec)
  tolerance <- 31*tolerance
  return(ifelse(abs(time_vec-shift(time_vec, n=n, fill=fill, type=type)) > tolerance,fill,
                shift(x, n=n, fill=fill, type=type)))
}

# in case the above turns out to be too inefficient expand this one: 
rshift <- function(x, n=1){
  indx <- (1+n):(length(x)+n) #  shift vector, adds NAs for lag
  indx[indx<1] <- NA # in case of lead add NAs
  return(x[indx]) # return adjusted x
}


# function to calculate the FIRST mode
first_mode <- function(vec, remove.NA = TRUE, char_type = TRUE){
  if (remove.NA == TRUE) vec <- vec[!is.na(vec)]
  if (length(vec) == 0) {if (char_type==TRUE) return(NA_character_) else return(NA)} # this is to ensure that NA is return if vec is empty
  vec_uniq <- unique(vec) 
  return(vec_uniq[which.max(tabulate(match(vec,vec_uniq)))])
}


# function to calculate the frequency of the FIRST mode
N_first_mode <- function(vec, remove.NA = TRUE){
  if (remove.NA == TRUE) vec <- vec[!is.na(vec)]
  if (length(vec) == 0) return(NA_integer_) # this is to ensure that NA is return if vec is empty
  vec_uniq <- unique(vec) 
  return(as.integer(max(tabulate(match(vec,vec_uniq))))) 
}