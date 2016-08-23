# script to identify flow paths of parts

########################################
##### aggregate at the Station level ###
########################################

library(data.table)
library(readr)

# get categorical data
catcols <- fread("train_categorical.csv", nrows = 0L)  
classvector <- c("integer", rep.int("character", ncol(catcols)-1)) 
cats <- fread("train_categorical.csv", colClasses = classvector, nrows = 60L, skip=0L) # use this line for part 1
# cats <- fread("train_categorical.csv", colClasses = classvector, nrows = -1L, skip=61L) # use these lines for part 2:n
# headses <- colnames(catcols)
# setnames(cats, headses)

# reshape and get stations for aggregating
cats = melt(cats, 'Id', variable.name='feature',  value.name='measurement')
cats[, station := substr(feature, 1,6)]
cats[, feature := NULL] 

# get numeric data
numcols <- fread("train_numeric.csv", nrows = 0L)  
classvector <- c("integer", rep.int("character", ncol(numcols)-1)) 
nums <- fread("train_numeric.csv", colClasses = classvector, nrows = 60L, skip=0L)  # use this line for part1
# nums <- fread("train_numeric.csv", colClasses = classvector, skip=61L, nrows = -1L)  # use these lines for part 2:n
# headses <- colnames(numcols)
# setnames(nums, headses)

# reshape and get stations for aggregating
nums[, Response := NULL] # we can add this back in later
nums = melt(nums, 'Id', variable.name='feature',  value.name='measurement')
nums[, station := substr(feature, 1,6)]
nums[, feature := NULL] 

# put it together into one long skinny table
parts <- rbind(cats, nums)
rm(nums)
rm(cats)
gc()

# aggregate at the station level
partssum <- parts[, .(meas = base::mean(as.numeric(measurement), na.rm = TRUE)), by= .(station, Id)]
# partssum <- parts[, .(meas = mean(as.numeric(measurement), na.rm = TRUE)), by= .(station, Id)]
rm(parts)
gc()

#re-reshape and clean up
partssum =  dcast(partssum, Id ~ station, value.var="meas")
setnames(partssum, c("L0_S0_", "L0_S1_", "L0_S2_", "L0_S3_", "L0_S4_", "L0_S5_"
                 , "L0_S6_" , "L0_S7_", "L0_S8_", "L0_S9_")
               , c("L0_S00", "L0_S01", "L0_S02", "L0_S03", "L0_S04", "L0_S05" 
                 , "L0_S06", "L0_S07", "L0_S08", "L0_S09")
        )

write_csv(partssum, "prodfams1.csv") # i need to get new beta version of data.table with fwrite!



###################################
##### produce the Visualization ###
###################################

setDF(partssum)
library(VIM)

png(filename="flowpaths.png",  # use this device for scalable, high-res graphics
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

# show the data by volume
miceplot <- aggr(partssum[, -c(1)], col=c("dodgerblue","lightgray"),
                 numbers=TRUE, combined=TRUE, varheight=TRUE, border=NA,
                 sortVars=FALSE, sortCombs=FALSE, ylabs=c("Product Families"),
                 labels=names(partssum[, 1]), cex.axis=.7)
dev.off()

