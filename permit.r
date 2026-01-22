setwd("C:/Users/robert.ellis/Desktop/Projects/Permit/Data")
library(MASS)
library(vegan)
library(lme4)
library(plyr)
library(car)
library(fishualize)
#library(dplyr)		
#library(ggplot2)
#library(stringr)
library(tidyverse) # just get this one, includes the ones above
library(lubridate)	
library(vidiris)
library(scales)

#------------------------------------------------------------
# Tidyverse cheatsheets:
# https://rstudio.com/resources/cheatsheets/ 
# https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
# https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
# https://github.com/rstudio/cheatsheets/raw/master/lubridate.pdf
#------------------------------------------------------------
# Helpful websites for tidyverse:
# https://dplyr.tidyverse.org/
# https://dplyr.tidyverse.org/reference/summarise.html
# https://www.guru99.com/r-aggregate-function.html#1
#------------------------------------------------------------
# Helpful websites for ggplot2:
# https://ggplot2.tidyverse.org/index.html
# http://www.sthda.com/english/wiki/ggplot2-essentials
# https://www.r-graph-gallery.com/index.html
# http://rforpublichealth.blogspot.com/2013/11/ggplot2-cheatsheet-for-scatterplots.html
#------------------------------------------------------------

# Read in the data file(s)
# Use your own version of file location management
# This is not the best way, it's simple and dumb but it works
# Use the setwd command to set your working directory,
# here I set it to a local folder that includes copies of the data files

setwd("C:/Users/robert.ellis/Desktop/Projects/Permit/Data")

# Read in files with read.csv
# Check that the df is read in correctly with functions dim() and head() 

perm2018 <- read.csv (file = "tbperm_matched_detections_2018.csv",header=T)
dim(perm2018) 	# 67215		36
head(perm2018)
# head() shows that there is more here than we need, so let's drop extraneous columns
str(perm2018) 
# str() returns type of each vector in list format, easier to keep count
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm18 <- perm2018[-c(1:7,11,13:18,23:36)]
# double check that the new df only has what we want
dim(perm18) 		# 67215		8
#head(perm18)
# Format datecollected
perm18$datecollected <- ymd_hms(perm18$datecollected)
str(perm18) 

# Repeat for 2019-2021 
perm2019 <- read.csv (file = "tbperm_matched_detections_2019.csv",header=T)
dim(perm2019) 	# 157721	34
head(perm2019)
str(perm2019) # there are two columns in the 2018 data that are not here, so keep code is shorter
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm19 <- perm2019[-c(1:7,11,13:18,23:34)]
dim(perm19) 		# 157721		8
#head(perm19)
# Format datecollected 
perm19$datecollected <- ymd_hms(perm19$datecollected)
str(perm19)

perm2020 <- read.csv (file = "tbperm_matched_detections_2020.csv",header=T)
dim(perm2020) 	# 156990		34
head(perm2020)
str(perm2020) 
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm20 <- perm2020[-c(1:7,11,13:18,23:34)]
dim(perm20) 		# 156990		8
#head(perm20)
# Format datecollected 
perm20$datecollected <- ymd_hms(perm20$datecollected)
str(perm20)

perm2021 <- read.csv (file = "tbperm_matched_detections_2021.csv",header=T)
dim(perm2021) 	# 520		34
head(perm2021)
str(perm2021) 
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm21 <- perm2021[-c(1:7,11,13:18,23:34)]
dim(perm21) 		# 520		8
#head(perm21)
# Format datecollected 
perm21$datecollected <- ymd_hms(perm21$datecollected)
str(perm21)

perm.2021.2 <- read.csv (file = "VUE_Export_TBOFF+HOG.csv")
dim(perm.2021.2) 	# 72320		8
head(perm.2021.2)
str(perm.2021.2) 
# drop first column
perm21.2 <- perm.2021.2[-c(1)]
str(perm21.2)
# Format datecollected 
perm21.2$datecollected <- mdy_hm(perm21.2$datecollected)
str(perm21.2)

perm2019.2 <- read.csv (file = "tbperm_unmatched_detections_2019.csv",header=T)
dim(perm2019.2) 	# 6452		31
#head(perm2019.2)
str(perm2019.2) # there are two columns in the 2018 data that are not here, so keep code is shorter
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm19.2 <- perm2019.2[-c(1:7,11,13:18,23:31)]
dim(perm19.2) 		# 6452		8
#head(perm19)
# Format datecollected 
perm19.2$datecollected <- mdy_hm(perm19.2$datecollected)
str(perm19.2)

perm2020.2 <- read.csv (file = "tbperm_unmatched_detections_2020.csv",header=T)
dim(perm2020.2) 	# 9716		31
#head(perm2020.2)
str(perm2020.2) # there are two columns in the 2018 data that are not here, so keep code is shorter
# Keep: 8-station, 9-receiver, 10-bottom_depth, 12-tagname, 19-datecollected, 20-timezone, 21-longitude, 22-latitude
perm20.2 <- perm2020.2[-c(1:7,11,13:18,23:31)]
dim(perm20.2) 		# 9716		8
#head(perm19)
# Format datecollected 
perm20.2$datecollected <- mdy_hm(perm20.2$datecollected)
str(perm20.2)


# Combine the dataframes with the do.call function 
perm.all <- do.call("rbind", list(perm18, perm19, perm19.2, perm20, perm20.2, perm21, perm21.2))
# Check that it combined correctly
dim(perm.all) 	# 470934		8
head(perm.all)
str(perm.all)

#------------------------------------------------------------------
# UPDATE IF MORE INFO 
# Add tag info
# step 1: read in .csv 
tag.info <- read.csv (file = "tag.info.csv",header=T)
dim(tag.info) 		# 17		4
# step 2: merge with perm.all
perm.1 <- merge(perm.all, tag.info, by = "tagname")
dim(perm.1)		# 470734		11

# Check that all vectors are in correct format
str(perm.1)
# Convert tag date & exp date to dates
perm.1$tagdate <- mdy(perm.1$tagdate)
perm.1$exp_date <- mdy(perm.1$exp_date)

#------------------------------------------------------------------
#------------------------------------------------------------------

# Now we have a full dataframe - perm.1
# Cleaning steps: fix time
# Filtering steps:  & cleaning steps - drop "release", false detection

# Format the datetime columns (very important for later)
# step 1: change datecollected column from chr to POSIXct
# We already did this when reading in each file
#perm.1$datecollected <- ymd_hms(perm.all$datecollected, tz="UTC")
# step 2: rename the datecollected column
colnames(perm.1)[5]<-"DT_UTC"
# step 3: create column for local time (ET) that accounts for DST
perm.1$DT_EST <- with_tz(perm.1$DT_UTC,tzone="America/New_York")
# step 4: create a new column of date only, drop time
perm.1$dates <- as.Date(perm.1$DT_EST, "%Y-%m-%d")

# KEEP perm.1 SAFE!!!

#------------------------------------------------------------------
# Next step is not neccessary after merging with tag.info!
# Need to remove tag 18360 - tarpon not permit!
#perm.2 <- perm.1 %>% filter(!(tagname == 'A69-9001-18360'))
#dim(perm.1)		# 454566		11

# Drop all "release" rows
perm.2 <- subset(perm.1, perm.1$receiver != "release")
dim(perm.2) 	# 470674   	13

# Looks like it dropped 60 rows... does that make sense? Yes, 4 years of data on 15 fish = 60.
# To see all the "release" rows, use this:
release <- subset(perm.all, perm.all$receiver == "release")
#------------------------------------------------------------------

# Check for duplicate detections, here that means same fish, same receiver, same time
dups <- duplicated(perm.2) 	# WARNING - this takes a while... or may not work at all...
table(dups)	 # No duplicates! Whew!

# Code to drop dups if any TRUE
#perm.1$dups <- dups
#perm.2 <- subset(perm.1,(dups=="TRUE"))
#dim(perm.2)	 
# clean it up (drop "dups")
#perm.3 <- perm.3[-c(XXX)]

# Check for duplicate stations - some were close together
# Extract a list of stations with detections
unique(perm.2$station)
# Better, use tidyverse to summarize by station then write to csv file
site <- perm.2 %>% group_by(station, longitude, latitude) %>% summarise(dets = n()) %>% arrange(desc(dets))
write.csv(site,"stations.csv")
# Need to combine detections from DOC & DOCN using gsub()
perm.2$station <- gsub("DOCN", "DOC", perm.2$station)
# Double check, re-run 
site <- perm.2 %>% group_by(station, longitude, latitude) %>% summarise(dets = n()) %>% arrange(desc(dets))
write.csv(site,"stations.csv")
# DOC showing up as 1 station with 2 locations
# Replace lat/long from DOCN with DOC lat/long
perm.2["latitude"][perm.2["latitude"] == 27.5073] <- 27.5064
perm.2["longitude"][perm.2["longitude"] == -82.9854] <- -82.98472
# check 
site <- perm.2 %>% group_by(station, longitude, latitude) %>% summarise(dets = n()) %>% arrange(desc(dets))
write.csv(site,"stations.csv")
# repeat steps to merge ACJL
perm.2["latitude"][perm.2["latitude"] == 27.48702] <- 27.488
perm.2["latitude"][perm.2["latitude"] == 27.48712] <- 27.488
perm.2["latitude"][perm.2["latitude"] == 27.48847] <- 27.488
perm.2["latitude"][perm.2["latitude"] == 27.48883] <- 27.488
perm.2["longitude"][perm.2["longitude"] == -82.98437] <- -82.984
perm.2["longitude"][perm.2["longitude"] == -82.9851] <- -82.984
perm.2["longitude"][perm.2["longitude"] == -82.99] <- -82.984
perm.2["longitude"][perm.2["longitude"] == -82.98502] <- -82.984
perm.2$station <- gsub("SL", "ACJL", perm.2$station)
perm.2$station <- gsub("ML", "ACJL", perm.2$station)
perm.2$station <- gsub("MS", "ACJL", perm.2$station)
perm.2$station <- gsub("SS", "ACJL", perm.2$station)
perm.2$station <- gsub("NL", "ACJL", perm.2$station)
perm.2$station <- gsub("NS", "ACJL", perm.2$station)
# check
site <- perm.2 %>% group_by(station, longitude, latitude) %>% summarise(dets = n()) %>% arrange(desc(dets))
write.csv(site,"stations.csv")
# weird but subbing ACJL for SL turned HRSL into HRACJL
# change it back
perm.2$station <- gsub("HRACJL", "HRS", perm.2$station)
# check
site <- perm.2 %>% group_by(station, longitude, latitude) %>% summarise(dets = n()) %>% arrange(desc(dets))
write.csv(site,"stations.csv") 
# should have 54 unique stations - double check the fixes above to DOC, ACJL, HRS 


#rerun the duplicate check (shouldn't find any)
dups <- duplicated(perm.2) 	
table(dups)	 # whew! still no duplicates!

# False detection filter
# To start, let's define any solitary detection within an hour bin (3600 s) as false
# First find the difference in time(s) between detections
# reorder by time, newest to oldest
perm.3 <- perm.2[order(perm.2$tagname,perm.2$DT_UTC, decreasing = F),]  

# Run this function that checks next row (1) or preceding (-1)
rowshift <- function(x, shiftLen = 1L) 
  {             
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])    
  }

# group by transmitter and make new column called diff = seconds between row and previous
perm.3 <- perm.3 %>% group_by(tagname) %>% mutate(diff = difftime(DT_UTC, rowshift(DT_UTC,-1),tz=UTC,units=c("secs")))

# check the mean time difference between detections
perm.3 %>% group_by(tagname) %>% summarise(mean=mean(diff,na.rm=T),sd=sd(diff,na.rm=T))

# remove any solitary detection within 1h(3600s)
# first 
perm.4 <- perm.3 %>% group_by(tagname) %>% mutate(false=ifelse(is.na(diff) & rowshift(diff,1) <3600, 0,
	ifelse(diff <3600, 0,
	ifelse(diff>3600 & rowshift(diff,1)<3600,0,1))))
table(perm.4$false)	# 546 false detections = 0.12% = VERY low (even worth it to remove them??)

# If we decide to drop them, first filter out the false rows  
perm.4 <- perm.4 %>% filter(false==0)
# then turn the tibble back to a dataframe
perm.4 <- as.data.frame(perm.4)		
# last, drop the "false" column (but keep "diff" for now) and check to see everything is in it's right place
perm.4 <- perm.4[-c(14)]
dim(perm.4)		# 470,126 == # of filtered & checked detections

# SAVE PROGRESS
write.csv(perm.4,"perm4.csv", row.names = FALSE)


#------------------------------------------------------------------------------
# SHORTCUT TO HERE USING VALIDATED perm.4

perm.4 <- read.csv(file = "perm.4.csv")
str(perm.4)

# Need to format vars
perm.4$receiver <- as.character(perm.4$receiver)
perm.4$DT_UTC <- as.POSIXct(perm.4$DT_UTC)
perm.4$DT_EST <- as.POSIXct(perm.4$DT_EST,tz = "America/New_York")

perm.4$tagdate <- ymd(perm.4$tagdate)
perm.4$exp_date <- ymd(perm.4$exp_date)
perm.4$dates <- ymd(perm.4$dates)
perm.4$false <- as.integer(perm.4$false)

#------------------------------------------------------------------------------

# Now we have a clean & filtered DF = perm.4
# Start summarizing 

# Number of times each fish is detected and how many unqiue tags you have
table(perm.4$tagname) 	# Returns list of tag ID (name) with the number of rows it appears in 
unique(perm.4$tagname)	# Returns list of unique tags = 13; hmm, filtering dropped four of the tags!

# Where did we lose them? Compare table(perm.x$tagname)
table(perm.all$tagname) 	# unfiltered == 17
table(perm.1$tagname)		# lost 1 (tarpon that wasn't supposed to be there)
table(perm.2$tagname)		# drop release rows = lost 3
table(perm.4$tagname)		# drop false detects = lost 1 (#6555 only had 1 detection)

# In case we want to look at what else the false detection filter kicked out
# Re-run the filter because we already dropped them...
perm.3 <- perm.2 %>% group_by(tagname) %>% mutate(false=ifelse(is.na(diff) & rowshift(diff,1) <3600, 0,
	ifelse(diff <3600, 0,
	ifelse(diff>3600 & rowshift(diff,1)<3600,0,1))))
table(perm.3$false)	#428 false detections = 0.11% = VERY low (even worth it to remove them??)
false <- subset(perm.3, perm.3$false == "1")
write.csv(false,"false.csv")
unique(false$tagname)


# Back to summarizing...
# Makes a tibble ordered by tag (name)
by_name <- perm.4 %>% group_by(tagname)
# And this makes a tibble ordered by reciever station
by_station <- perm.4 %>% group_by(station, latitude, longitude)

# The real power of tidyverse is in combining functions in the same line of code
# These both give us a version of detections (dets) by tag, the 2nd one in descending order
perm.4 %>% count(tagname)
tag <- by_name %>% summarise(dets = n()) %>% arrange(desc(dets))

# And these both give us detections by station
perm.4 %>% count(station)
site <- by_station %>% summarise(dets = n()) %>% arrange(desc(dets))
# we did this already, but re-do it here with filtered detections - filtering dropped 5 stations!
write.csv(site,"stations.csv")	
lsite <- mutate(site, ldets = log(dets))	# log(dets) for mapping later

# You can also combine groupings:
# This makes tibbles that list detections by tag by station... not really useful yet
by_name_st <- perm.4 %>% group_by(tagname, station)
by_st <- by_name_st %>% summarise(dets = n())

# Now let's get detections by date over time:
# Make a tibble of detections by date, not very useful
det.date <- perm.4 %>% group_by(dates) %>% summarize(dets = n())

# BUT now just one more step to get "detection days"
# summarize by Date, Station, Name (tag)
ab <- perm.4 %>% group_by(dates, tagname, station) %>% summarize(dets = n())
# export to .csv (for later maybe)
write.csv(ab,"abacus.csv")

# Some more detection days & summary types
dd.site <- ab %>% group_by(station) %>% summarize(dd = n())
dd.tag <- ab %>% group_by(tagname) %>% summarize(dd = n())

# days at large - get last day detected
dal <- ab %>% group_by(tagname) %>% summarize(dal = max(dates))

# total detects per tag
t.dets <- perm.4 %>% group_by(tagname) %>% summarize(dets = n())

# total sites per tag
t.sites <- perm.4 %>% group_by(tagname, station) %>% summarize(dets = n())
t.s <- t.sites %>% group_by(station) %>% summarize(locs = n())

# max / min latitude
lat <- perm.4 %>% group_by(tagname, latitude) %>% summarize(lat = n())
mm.l <- lat %>% group_by(tagname) %>% summarize(max.l = max(latitude), min.l = min(latitude))


# Why bother with detection days if you're not going to make an abacus plot?
## Code generously shared by Jenny Herbig ##

#                Abacus Plot                   #

# This will make a basic abacus plot, all fish, one color
ppi <- 300
png("abacus.png", width=9*ppi, height=9*ppi, res=ppi)

dotplot(reorder(perm.4$tagname,perm.4$DT_EST) ~ perm.4$DT_EST, data=perm.4, labels=row.names(perm.4$tagname), cex=.7,
			main="Permit Filtered Detections",  xlab="Date" , ylab="Transmitter", scales=list(format="%b %Y", tick.number=20, rot=45)) 

dev.off()

# Let's add some station info - color by bottom depth
# Make 4 groups, <10m, 10-20m, 20-30m, 30m+
# Use this to get the colors in viridis pallette, here X = 4: show_col(viridis_pal()(X))
show_col(viridis_pal()(4))
perm.4$depth <- cut(perm.4$bottom_depth,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))

perm.4$region <- cut(perm.4$latitude,c(0,25.5,27,30),labels=c("SPZ","CH","TB"))

ppi <- 300
png("abacus2.png", width=6*ppi, height=6*ppi, res=ppi)

dotplot(reorder(perm.4$fishnum,perm.4$tagdate) ~ perm.4$DT_EST, data=perm.4, labels=row.names(perm.4$fishnum), cex=1.1, 
	groups=factor(perm.4$depth, labels= c("<10m","10-20m","20-30m",">30m")), col= c('#440154FF','#31688EFF','#35B779FF','#FDE725FF'), xlab="Date" , ylab="Fish ID",
		scales=list(format="%b %Y", tick.number=20, rot=45), pch=16,
			key=list(corner = c(0.025,0.975), columns= 1, border = TRUE, transparent = TRUE, title = "Depth", cex=0.9, cex.title = 0.85, text=list(c("<10m","10-20m","20-30m",">30m"), col=c('#440154FF','#31688EFF','#35B779FF','#FDE725FF'))))
dev.off()

ppi <- 300
png("abacus3.png", width=6*ppi, height=6*ppi, res=ppi)

dotplot(reorder(perm.4$fishnum,perm.4$tagdate) ~ perm.4$DT_EST, data=perm.4, labels=row.names(perm.4$fishnum), cex=1.1, 
	groups=factor(perm.4$region, labels= c("SPZ","CH","TB")), col= c('#FDE725FF','#440154FF','#31688EFF'), xlab="Date" , ylab="Fish ID",
		scales=list(format="%b %Y", tick.number=20, rot=45), pch=16,
			key=list(corner = c(0.025,0.975), columns= 1, border = TRUE, transparent = TRUE, title = "Region", cex=0.9, cex.title = 0.8, text=list(c("TB","CH","SPZ"), col=c('#31688EFF','#440154FF','#FDE725FF'))))
dev.off()


# To make abacus plot with ggplot
# Need to make tagdate into POSIXct format
perm.4$tagdate <- as.POSIXct(perm.4$tagdate)

abacus <- ggplot() +
	geom_point(data = perm.4, aes(DT_EST, y = tagname, color = factor(depth)), cex = 3, pch = 21) +
	geom_point(data = perm.4, aes(tagdate, y = tagname), cex = 2.5, pch = 17, color = "black") +
	scale_colour_manual("Depth (m)", values=c('#440154FF','#31688EFF','#35B779FF','#FDE725FF')) +
	scale_x_datetime(breaks = date_breaks("4 months"), labels = date_format("%b-%y"),
         limits = as.POSIXct(c("2018-05-01 00:00:00", "2021-12-31 00:00:00"))) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Transmitter", title = "Tampa Bay Permit Filtered Detections")

plot(abacus)
  

#------------------------------------------------------------------------------

# Generate detections by month:
# ...this was actually a tricky problem... found help here:
# https://stackoverflow.com/questions/46691933/r-sort-by-year-then-month-in-ggplot2
# First, create new columns of year & year-month with function(mutate)
# then use group_by and summarize to make a tibble of detections by month
dets.month <- perm.4 %>% mutate(year = format(DT_UTC, "%Y"), yrmon = format(DT_UTC, "%Y-%m"), month = format(DT_UTC, "%m")) %>% group_by(year, yrmon, month) %>% summarize(dets = n())

# Make a tibble with the number of tags detected per month
# then use left_join to combine tibbles
tags.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon) %>% summarise(tags = n_distinct(tagname))
tags.mon <- left_join(dets.month,tags.month, by = "yrmon") 

# Now we have something to plot: detections over time, by year-month
# ggplot uses multiple calls combined with "+" to add levels of complexity to a plot
# Example of a simple bar plot of detections (y) per month (x):
ggplot(tags.mon, aes(yrmon, dets)) +
    geom_col ()
	
ggplot(tags.mon, aes(yrmon, tags)) +
    geom_col ()

# A few steps ago made a column of "year" so let's add some color, fix x-axis labels, add # tags above, etc. 
ggplot(tags.mon, aes(yrmon, dets, fill = year)) +
    geom_col () + 
	geom_text(aes(label=tags), vjust = -0.5) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_viridis_d("Year") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "# detections", title = "Total detections per month") 

# Better log-transformed? Ugh, nope...
ggplot(tags.mon, aes(yrmon, dets, fill = year)) +
    geom_col () + 
	scale_y_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","100","1,000","10,000","100,000")) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_viridis_d("Year") +
	geom_text(aes(label=tags), vjust = -1) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "# detections", title = "Log-transformed detections per month")

# Total detections is slightly biased, try with DD instead
# DD / month:
dd.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon, tagname, station) %>% summarize(dets = n())
dd.mon <- dd.month %>% group_by(yrmon,tagname) %>% summarize(dd = n())

ggplot(dd.mon, aes(yrmon, dd)) +
    geom_col ()
	
# Now add depth bins
# perm.4 already has a depth bin column, so keep this in the DD above	
perm.4$depth <- cut(perm.4$bottom_depth,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))

dd.month <- perm.4 %>% mutate(month = format(DT_UTC, "%m")) %>% group_by(month, tagname, depth) %>% summarize(dets = n())
dd.mon <- dd.month %>% group_by(month, depth) %>% summarize(dd = n())

ggplot(dd.mon, aes(x = month, y = dd, fill = depth)) +
	geom_col() +
	scale_fill_viridis_d("Depth") +
	scale_x_discrete(labels=month.abb) +
    labs(x = "Month", y = "Days detected") +
	theme_bw()

# read in the mark-recapture data
mark <- read.csv (file = "mark_recapture.csv",header=T)
dim(mark) 	# 223		5
str(mark)
mark$date <- mdy(mark$date)
mark$depth <- as.factor(mark$depth)

mark.month <- mark %>% mutate(month = format(date, "%m")) %>% group_by(month, depth) %>% summarize(dets = n())
#mark.mon <- mark.month %>% group_by(month, depth) %>% summarize(dd = n())

cols <- c("<10m" = "#440154FF", "10-20m" = "#31688EFF", "20-30m" = "#35B779FF", ">30m" = "#FDE725FF", "na" = "grey", " " = "white")

ggplot(mark.month, aes(x = month, y = dets, fill = depth)) +
	geom_col() +
	scale_fill_manual("Depth", values = cols) +
	scale_x_discrete(labels=month.abb,drop=FALSE) +
    labs(x = "Month", y = "Tags deployed") +
	theme_bw()

ggplot(sites.mon, aes(yrmon, sites, fill = season)) +
    geom_col (alpha = 0.85) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_manual("Season",values = cols) +


# Now let's explore some station data 	
# Make a tibble of number of stations visted per month by all fish & plot
sites.mon <- perm.4 %>% mutate(year = format(DT_UTC, "%Y"),yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(year, yrmon) %>% summarise(sites = n_distinct(station))
ggplot(sites.mon, aes(yrmon, sites, fill=year)) +
    geom_col () +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_viridis_d("Year") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "# sites", title = "Sites with detections per month")
	
# Explore seasonality in the data 
# Generate colum of seasons, first a function:
seasons = function(x){
  if(x %in% 2:4) return("Spring")
  if(x %in% 5:7) return("Summer")
  if(x %in% 8:10) return("Fall")
  if(x %in% c(11,12,1)) return("Winter")}
# Next create column, apply function to the dates column
perm.4$season <- sapply(month(perm.4$dates), seasons)

# Re-create stations by month plot, colored by season instead of year
sites.mon <- perm.4 %>% mutate(year = format(DT_UTC, "%Y"),yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(year, yrmon, season) %>% summarise(sites = n_distinct(station))
cols <- c("Spring" = "#35B779FF", "Summer" = "#FDE725FF", "Fall" = "#31688EFF", "Winter" = "#440154FF")
ggplot(sites.mon, aes(yrmon, sites, fill = season)) +
    geom_col (alpha = 0.85) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_manual("Season",values = cols) +
	#scale_fill_viridis_d("Season") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "# sites", title = "Sites with detections per month")

# Tags by month plot, colored by season
tag.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon,season) %>% summarise(tags = n_distinct(tagname))
show_col(viridis_pal(option = "plasma")(4))

pcols <- c("Spring" = "#F0F921FF", "Summer" = "#ED7953FF", "Fall" = "#9C179EFF", "Winter" = "#0D0887FF")
tag.season <- ggplot(tag.month, aes(yrmon, tags, fill = season)) +
    geom_col (alpha = 0.75) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_fill_manual("Season",values = pcols) +
	#scale_fill_viridis_d("Season",option="plasma") +
	theme_bw() +
	theme(axis.text.x = element_blank()) +
    labs(y = "# tags")


# Calculate the mean location (latitude) for all fish by month 
loc.month <- perm.4 %>% mutate(year = format(DT_UTC, "%Y"),yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(year,yrmon) %>% summarise(loc = mean(latitude),loc.e = sd(latitude))
loc.mon <- left_join(loc.month,tags.month, by = "yrmon") 

# Location is continuous so switch to a line plot, add points, add tags, use theme_classic to clean it up
ggplot(loc.mon, aes(yrmon, loc, group = 1)) +
	geom_line() + 
	geom_point() +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	geom_text(aes(label=tags), vjust = -1) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude", title = "Mean latitude of detected fish per month")

# Not great without error...
loc.mon$tags <- as.factor(loc.mon$tags)

ggplot(loc.mon, aes(yrmon, loc, group=1, color = loc.mon$tags)) +
	geom_point(data = loc.mon, aes(show.legend = T, size = tags, color = tags)) +
	geom_errorbar(aes(ymin = loc-loc.e, ymax = loc+loc.e), width=.5) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_size_discrete("# Tags", range=c(1,7)) +
	scale_color_viridis_d("# Tags") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude", title = "Mean latitude of all detected fish by month")

# Try something else... dot plot showing range (monthly location by tag)... close but not quite
dd.tag <- ab %>% mutate(yrmon = format(dates, "%Y-%m")) %>% group_by(yrmon, tagname) %>% summarize(dd = n())

#dd.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon, tagname, station, season) %>% summarize(dets = n())
#dd.mon <- dd.month %>% group_by(yrmon,tagname,season) %>% summarize(dd = n())

fish.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon,tagname,season) %>% summarise(loc = mean(latitude),loc.e = sd(latitude))
fish.mon <- left_join(fish.month,dd.tag) 
#fish.mon$tags <- as.factor(fish.mon$tags)

ggplot(fish.mon, aes(yrmon, loc, fill = year, color = year)) +
	geom_dotplot(binaxis = 'y', stacdir = 'center',dotsize=0.5,stackratio = 0.25) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_y_continuous(breaks=seq(25,28,by=0.5)) +
	scale_color_viridis_d("Year") +
	scale_fill_viridis_d("Year") +
	#stat_summary(fun.data = mean_sdl, fun.args =list(mult=1),geom="pointrange", color = "red", show.legend = F) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude", title = "Mean latitude of all detected fish")

# Bubble plots
 
# bubble plot with color = season, size = DD

dd.tag <- ab %>% mutate(yrmon = format(dates, "%Y-%m")) %>% group_by(yrmon, tagname) %>% summarize(dd = n())
fish.month <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon,tagname,season) %>% summarise(loc = mean(latitude),loc.e = sd(latitude))
fish.mon <- left_join(fish.month,dd.tag) 

# color by season from earlier plot
cols <- c("Spring" = "#35B779FF", "Summer" = "#FDE725FF", "Fall" = "#31688EFF", "Winter" = "#440154FF")

dd.season <- ggplot(fish.mon, aes(yrmon, loc, fill = season, color = season, size = dd)) +
	geom_point(alpha=0.5, shape = 21) +
	geom_hline(yintercept=25.15, linetype="dashed", color = "black", size=0.5) +
	scale_y_continuous(breaks=seq(25,28,by=0.5)) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_size_continuous("# DD",range=c(1,10)) +
	scale_color_manual("Season",values = cols) +
	scale_fill_manual("Season",values = cols) +
	guides(color = guide_legend(override.aes = list(size=5))) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude") +
	theme(legend.position="top")

# bubble plot with color = depth, size = DD
# depth bins from earlier
perm.4$depth <- cut(perm.4$bottom_depth,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))
# need to keep depth in dd by month, first step is to get list of days with detections (above we did this with ab)
# change to get n_days in month with detections = dm (NOT dd)
ab.2 <- perm.4 %>% group_by(dates, tagname) %>% summarize(dets = n())
# next turn this into a tibble of dm by tag
ddz <- ab.2 %>% mutate(yrmon = format(dates, "%Y-%m")) %>% group_by(yrmon, tagname) %>% summarize(dm = n())
# now make a tibble of mean latitude & depth by fish for each month & join
locz <- perm.4 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon,tagname) %>% summarise(loc = mean(latitude),z = mean(bottom_depth))
perm.mon <- left_join(ddz,locz) 
perm.mon$dep <- cut(perm.mon$z,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))

# bubble plot with color = depth, size = DD
zcol <- c("<10m" = "#440154FF", "10-20m" = "#31688EFF", "20-30m" = "#35B779FF", ">30m" = "#FDE725FF")
#zcol <- c("<10m" = "#35B779FF", "10-20m" = "#FDE725FF", "20-30m" = "#31688EFF", ">30m" = "#440154FF")

dd.color <- ggplot(perm.mon, aes(yrmon, loc, fill = dep, color = dep, size = dm)) +
	geom_point(alpha=0.5, shape = 21) +
	geom_hline(yintercept=25.15, linetype=2, color = "black", size=0.5) +
	geom_hline(yintercept=27, linetype=3, color = "black", size=0.5) +
	geom_hline(yintercept=25.8, linetype=3, color = "black", size=0.5) +
	annotate(geom="text", x=42, y=27.4, label="WF") +
	annotate(geom="text", x=42, y=26.4, label="SW") +
	annotate(geom="text", x=42, y=25.5, label="EV") +
	annotate(geom="text", x=42, y=25.0, label="SPZ") +
	scale_y_continuous(breaks=seq(25,28.6,by=0.4)) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_size_continuous("Days detected",range=c(1,10)) +
	scale_color_manual("Depth zone",values = zcol) +
	scale_fill_manual("Depth zone",values = zcol) +
	guides(color = guide_legend(override.aes = list(size=5))) +
	theme_classic() +
	theme(panel.border = element_rect(linetype = "solid", fill = NA),axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude")

ppi <- 600
tiff("fig3.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

dd.color

dev.off()


# Facet plot dd.season with rec  above

# vertical bar plot showing receiver # per 0.1 degree of latitude
recs <- read.csv (file = "recs_binned.csv",header=T)
dim(recs) 	# 29 	2
str(recs)

rec.bin <- ggplot(recs, aes(latitude, n.rec)) +
    geom_col () +
	theme_bw() +
	scale_x_continuous(breaks=seq(25,27.8,by=0.5)) +
	labs(y = "# recievers")# +	
	#theme(axis.text.y = element_blank()) 
  
rec.bin + coord_flip() 
	
	
# Calculate the mean depth for all fish by month 
z.month <- perm.4 %>% mutate(year = format(DT_UTC, "%Y"),yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(year,yrmon,season) %>% summarise(z = mean(bottom_depth),z.e = sd(bottom_depth))
z.mon <- left_join(z.month,tags.month, by = "yrmon") 
z.mon$tags <- as.factor(z.mon$tags)

ggplot(z.mon, aes(yrmon, z, group=1, color = z.mon$season)) +
	geom_point(data = z.mon, aes(show.legend =  T, size = tags, color = season, fill = season), alpha=0.85, shape = 21) +
	geom_errorbar(aes(ymin = z-z.e, ymax = z+z.e), width=.2) +
	scale_y_reverse() +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_size_discrete("# tags",range=c(1,7)) +
	scale_color_manual("Season",values = cols) +
	scale_fill_manual("Season",values = cols) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Depth (m)", title = "Mean depth (m) of all detected fish by month")
  


#------------------------------------------------------------------------------

# Making maps
#install.packages(c("rgeos"))

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rgdal)
library(ggspatial)
library(devtools)
library(tools)
library(raster)

# get basemap(s)
coastline10 <- ne_download(scale=10, type='coastline', category='physical',returnclass="sf")
countries10 <- ne_download(scale=10, type='countries', category='cultural',returnclass="sf")
eez <- ne_download(scale=10, type='admin_0_boundary_lines_maritime_indicator', category='cultural',returnclass="sf")
# this is another basemap, may need help to load
devtools::install_github("ropensci/rnaturalearthhires")
world <- ne_countries(scale = 10, returnclass = "sf")

# Get EEZ boundary from MarineRegions.com
# Get EEZ boundary shapefile from NOAA: https://www.nauticalcharts.noaa.gov/data/us-maritime-limits-and-boundaries.html
# Code from: https://rpubs.com/danielequs/marine_boundaries

# path to file
path.eez.noaa <- ("/Users/robert.ellis/Desktop/Projects/Permit/Data/NOAA_EEZ")
# filename of shape file
fnam.eez.noaa <- "USMaritimeLimitsNBoundaries.shp"
# read in shapefile using readOGR; file_path_sans_ext from library(tools)
eez.noaa <- readOGR(dsn = path.eez.noaa, layer = file_path_sans_ext(fnam.eez.noaa))

# Fortify the shapefile data:
# message: Regions defined for each Polygons
dat.eez.noaa <- fortify(eez.noaa) # a 10298x30 dataframe
str(dat.eez.noaa)

# test plot

p0 <- ggplot() +
    geom_sf(data = countries10) +
    coord_sf(xlim = c(-84, -79.5), ylim = c(24.5, 28.5)) +
	theme_bw()
p0

p1 <- p0 +
  geom_path(data = filter(eez.noaa, BOUND_ID == B0040),
            aes(x = long, y = lat, group = group), 
            colour = "red", size = 0.75)
p1

	geom_path(data = dat.eez.usa2, 
            aes(x = long, y = lat, group = group), 
            colour = "blue", size = 0.75) 
p1

# try using raster packages
setwd("C:/Users/robert.ellis/Desktop/Projects/Permit/Data/NOAA_EEZ")
noaa <- raster::shapefile("USMaritimeLimitsNBoundaries.shp")

# investigate spatial data
slotNames(noaa)
as_tibble(noaa@data)

# subset just the part of the EEZ we want
eez <- noaa %>% subset(., BOUND_ID == "B0040" | BOUND_ID == "B0035")

p0 <- ggplot() +
    geom_sf(data = countries10) +
    coord_sf(xlim = c(-84, -79.5), ylim = c(24.5, 28.5)) +
	theme_bw()
p0

p1 <- p0 +
  geom_path(data = eez,
			aes(x = long, y = lat, group = group), 
            colour = "red", size = 0.75)
p1

ggplot() +
	geom_polypath(data = noaa, fill = NA, color = "blue") +
	theme_void() +
	coord_map()

# read in spz boundary file then make a tibble
setwd("C:/Users/robert.ellis/Desktop/Projects/Permit/Data/")

spz <- read.csv (file = "spz.csv", header = T)
zone <- as_tibble(spz)
spz.a <- read.csv (file = "spz.a.csv", header = T)
zone.a <- as_tibble(spz.a)

# Generate detections by station, tags by station, both
site <- by_station %>% summarise(dets = n()) %>% arrange(desc(dets))
lsite <- mutate(site, ldets = log(dets))

tag_st <- by_station %>% summarize(dets = n(), tags = n_distinct(tagname))
site.t <- mutate(tag_st, ldets = log(dets))
site.t$tags <- as.factor(tag_st$tags)

ggplot() +
    geom_sf(data = countries10) +
    coord_sf(xlim = c(-84, -79.5), ylim = c(24.5, 28.5)) +
	geom_point(data = lsite, aes(x = longitude, y = latitude), size = lsite$ldets, 
        shape = 16, fill = "black") +
	theme_bw()


# Plot # tags by station

tagplot <- ggplot() +
    geom_sf(data = countries10) +
	annotation_scale(location = "bl", width_hint = 0.25, bar_cols = c("grey", "white")) +
    annotation_north_arrow(location = "tr", which_north = "true", height = unit(1.0, "cm"),
		width = unit(1.0, "cm"), pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
        style = north_arrow_orienteering) +
	coord_sf(xlim = c(-83.75, -79.75), ylim = c(24.5, 28.5)) +
	geom_point(data = site.t, aes(x = longitude, y = latitude, show.legend = T, size = tags, color = ldets, fill = ldets), 
        shape = 21) +
	geom_rect(data = tag_st, mapping = aes(xmin = -83.5, ymin = 27.0, xmax = -82.5, ymax = 28.0), fill = NA, color = "black", size = 0.6) +
	geom_path(data = eez, aes(x = long, y = lat, group = group), colour = "black", size = 0.5, linetype = 2) +
	scale_size_discrete("# tags", range=c(1,9)) +
	scale_color_viridis_c("Log detections") +
	scale_fill_viridis_c("Log detections") +
	theme_bw() +
	theme(axis.title = element_blank()) 

ppi <- 600
tiff("fig2a.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

tagplot + geom_line(data = zone, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.a, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.wf, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	geom_line(data = zone.sw, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	annotate("text", label = "West Florida [WF]", x = -83.75, y = 28.1, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "South West [SW]", x = -83, y = 26.3, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Everglades [EV]", x = -82.5, y = 25.5, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Florida Keys [FK]", x = -82.25, y = 24.9, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "SPZ Boundary", x = -83.75, y = 25, size = 5, hjust = 0, vjust = 0) +
	annotate("text", label = "Key West", x = -82, y = 24.45, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "Tampa Bay", x = -82.5, y = 27.6, size = 3, hjust = 0, vjust = 0.5) +
	annotate("text", label = "Biscayne Bay", x = -80.25, y = 25.55, size = 3, hjust = 0.5, vjust = 0.5) +
	annotate("text", label = "a.", x = -83.75, y = 28.5, size = 6, hjust = 0, vjust = 0.5) +
	theme(legend.position = "none")

dev.off()

# To use a different color scheme: scale_color_viridis_d("# Tags", option = "cividis") +

# Inset plot, color by transformed(dets), size by n_tag

tagplot2 <- ggplot() +
    geom_sf(data = countries10) +
	annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("grey", "white")) +
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.0, "cm"),
		width = unit(1.0, "cm"), pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
        style = north_arrow_orienteering) +
	coord_sf(xlim = c(-83.5, -82.4), ylim = c(27, 28)) +
	geom_point(data = site.t, aes(x = longitude, y = latitude, show.legend = T, size = tags, color = ldets, fill = ldets), 
        shape = 21) +
	scale_size_discrete("# tags", range=c(1,9)) +
	scale_color_viridis_c("Log detections") +
	scale_fill_viridis_c("Log detections") +
	theme_bw() +
	theme(axis.title = element_blank()) 

ppi <- 600
tiff("fig2b.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

tagplot2 +
	annotate("text", label = "Tampa", x = -82.65, y = 27.61, size = 4, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Bay", x = -82.65, y = 27.6, size = 4, hjust = 0.5, vjust = 1) +
	geom_line(data = zone.wf, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	annotate("text", label = "Sarasota", x = -82.55, y = 27.35, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "b.", x = -83.5, y = 28, size = 5, hjust = 0, vjust = 0.5) 

dev.off()

# Map of recaptures

mark <- read.csv (file = "mark_recaps.csv",header=T)
dim(mark)
str(mark)
mark$M_R <- as.factor(mark$M_R)

wf <- read.csv (file = "wf.csv", header = T)
zone.wf <- as_tibble(wf)
sw <- read.csv (file = "sw.csv", header = T)
zone.sw <- as_tibble(sw)


markplot <- ggplot() +
    geom_sf(data = countries10) +
	annotation_scale(location = "bl", width_hint = 0.25, bar_cols = c("grey", "white")) +
    annotation_north_arrow(location = "tr", which_north = "true", height = unit(1.0, "cm"),
		width = unit(1.0, "cm"), pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
        style = north_arrow_orienteering) +
	coord_sf(xlim = c(-83.75, -79.75), ylim = c(24.5, 28.5)) +
	geom_point(data = mark, aes(x = longitude, y = latitude, color = M_R), shape = 16, size = 3) +
	geom_path(data = eez, aes(x = long, y = lat, group = group), colour = "black", size = 0.5, linetype = 2) +
	scale_colour_manual("Tag location", values = c("#003f5c", "#ffa600"),labels=c('Within SPZ', 'Outside SPZ')) +
	theme_bw() +
	labs(x = "Longitude", y = "Latitude")

ppi <- 600
tiff("fig1.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

markplot + geom_line(data = zone, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.a, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.wf, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	geom_line(data = zone.sw, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	annotate("text", label = "SPZ Boundary", x = -83.75, y = 25, size = 5, hjust = 0, vjust = 0) +
	annotate("text", label = "Key West", x = -82, y = 24.45, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "Tampa Bay", x = -82.5, y = 27.6, size = 3, hjust = 0, vjust = 0.5) +
	annotate("text", label = "Biscayne Bay", x = -80.55, y = 25.55, size = 3, hjust = 0.5, vjust = 0.5) +
	annotate("text", label = "WF = 197 tags", x = -83.75, y = 27.7, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "SW = 175 tags", x = -83, y = 26.3, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "EV = 21 tags", x = -82.5, y = 25.5, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "FK = 515 tags", x = -82, y = 24.95, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "BB = ", x = -80.2, y = 25.3, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "443 tags", x = -80.15, y = 25.2, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "SE = ", x = -80.05, y = 26.4, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "137 tags", x = -80, y = 26.3, size = 3, hjust = 0, vjust = 0) 

dev.off()



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Filter out all TB and CH receivers 

perm.5 <- perm.4[!grepl("TB", perm.4$station),]
perm.5 <- perm.5[!grepl("CH", perm.5$station),]

# Re-make figures 2a & 2b and 3 without MERR data

# Generate detections by station, tags by station, both
by_station <- perm.5 %>% group_by(station, latitude, longitude)

site <- by_station %>% summarise(dets = n()) %>% arrange(desc(dets))
lsite <- mutate(site, ldets = log(dets))

tag_st <- by_station %>% summarize(dets = n(), tags = n_distinct(tagname))
site.t <- mutate(tag_st, ldets = log(dets))
site.t$tags <- as.factor(tag_st$tags)

ggplot() +
    geom_sf(data = countries10) +
    coord_sf(xlim = c(-84, -79.5), ylim = c(24.5, 28.5)) +
	geom_point(data = lsite, aes(x = longitude, y = latitude), size = lsite$ldets, 
        shape = 16, fill = "black") +
	theme_bw()


# Plot # tags by station

tagplot <- ggplot() +
    geom_sf(data = countries10) +
	annotation_scale(location = "bl", width_hint = 0.25, bar_cols = c("grey", "white")) +
    annotation_north_arrow(location = "tr", which_north = "true", height = unit(1.0, "cm"),
		width = unit(1.0, "cm"), pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
        style = north_arrow_orienteering) +
	coord_sf(xlim = c(-83.75, -79.75), ylim = c(24.5, 28.5)) +
	geom_point(data = site.t, aes(x = longitude, y = latitude, show.legend = T, size = tags, color = ldets, fill = ldets), 
        shape = 21) +
	geom_rect(data = tag_st, mapping = aes(xmin = -83.5, ymin = 27.0, xmax = -82.5, ymax = 28.0), fill = NA, color = "black", size = 0.6) +
	scale_size_discrete("# tags", range=c(1,9)) +
	scale_color_viridis_c("Log detections") +
	scale_fill_viridis_c("Log detections") +
	theme_bw() +
	theme(axis.title = element_blank()) 

ppi <- 600
tiff("fig2a.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

tagplot + geom_line(data = zone, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.a, aes(x = longitude, y = latitude), size = 0.5, linetype = 2) +
	geom_line(data = zone.wf, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	geom_line(data = zone.sw, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	annotate("text", label = "West Florida [WF]", x = -83.75, y = 28.1, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "South West [SW]", x = -83, y = 26.3, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Everglades [EV]", x = -82.5, y = 25.5, size = 3, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Florida Keys [FK]", x = -82.25, y = 24.9, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "SPZ Boundary", x = -83.75, y = 25, size = 5, hjust = 0, vjust = 0) +
	annotate("text", label = "Key West", x = -82, y = 24.45, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "Tampa Bay", x = -82.5, y = 27.6, size = 3, hjust = 0, vjust = 0.5) +
	annotate("text", label = "Biscayne Bay", x = -80.25, y = 25.55, size = 3, hjust = 0.5, vjust = 0.5) +
	annotate("text", label = "a.", x = -83.75, y = 28.5, size = 6, hjust = 0, vjust = 0.5) +
	theme(legend.position = "none")

dev.off()

# To use a different color scheme: scale_color_viridis_d("# Tags", option = "cividis") +

# Inset plot, color by transformed(dets), size by n_tag

tagplot2 <- ggplot() +
    geom_sf(data = countries10) +
	annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("grey", "white")) +
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.0, "cm"),
		width = unit(1.0, "cm"), pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
        style = north_arrow_orienteering) +
	coord_sf(xlim = c(-83.5, -82.4), ylim = c(27, 28)) +
	geom_point(data = site.t, aes(x = longitude, y = latitude, show.legend = T, size = tags, color = ldets, fill = ldets), 
        shape = 21) +
	scale_size_discrete("# tags", range=c(1,9)) +
	scale_color_viridis_c("Log detections") +
	scale_fill_viridis_c("Log detections") +
	theme_bw() +
	theme(axis.title = element_blank()) 

ppi <- 600
tiff("fig2b.tiff", width=6.5*ppi, height=6.5*ppi, res=ppi)

tagplot2 +
	annotate("text", label = "Tampa", x = -82.65, y = 27.61, size = 4, hjust = 0.5, vjust = 0) +
	annotate("text", label = "Bay", x = -82.65, y = 27.6, size = 4, hjust = 0.5, vjust = 1) +
	geom_line(data = zone.wf, aes(x = longitude, y = latitude), size = 0.5, linetype = 3) +
	annotate("text", label = "Sarasota", x = -82.55, y = 27.35, size = 3, hjust = 0, vjust = 0) +
	annotate("text", label = "b.", x = -83.5, y = 28, size = 5, hjust = 0, vjust = 0.5) 

dev.off()


# bubble plot with color = depth, size = DD
# depth bins from earlier
perm.5$depth <- cut(perm.5$bottom_depth,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))
# need to keep depth in dd by month, first step is to get list of days with detections (above we did this with ab)
# change to get n_days in month with detections = dm (NOT dd)
ab.2 <- perm.5 %>% group_by(dates, tagname) %>% summarize(dets = n())
# next turn this into a tibble of dm by tag
ddz <- ab.2 %>% mutate(yrmon = format(dates, "%Y-%m")) %>% group_by(yrmon, tagname) %>% summarize(dm = n())
# now make a tibble of mean latitude & depth by fish for each month & join
locz <- perm.5 %>% mutate(yrmon = format(DT_UTC, "%Y-%m")) %>% group_by(yrmon,tagname) %>% summarise(loc = mean(latitude),z = mean(bottom_depth))
perm.mon <- left_join(ddz,locz) 
perm.mon$dep <- cut(perm.mon$z,c(0,10,20,30,100),labels=c("<10m","10-20m","20-30m",">30m"))

# bubble plot with color = depth, size = DD
zcol <- c("<10m" = "#440154FF", "10-20m" = "#31688EFF", "20-30m" = "#35B779FF", ">30m" = "#FDE725FF")
#zcol <- c("<10m" = "#35B779FF", "10-20m" = "#FDE725FF", "20-30m" = "#31688EFF", ">30m" = "#440154FF")

dd.color <- ggplot(perm.mon, aes(yrmon, loc, fill = dep, color = dep, size = dm)) +
	geom_point(alpha=0.5, shape = 21) +
	geom_hline(yintercept=25.15, linetype=2, color = "black", size=0.5) +
	geom_hline(yintercept=27, linetype=3, color = "black", size=0.5) +
	geom_hline(yintercept=25.8, linetype=3, color = "black", size=0.5) +
	annotate(geom="text", x=42, y=27.4, label="WF") +
	annotate(geom="text", x=42, y=26.4, label="SW") +
	annotate(geom="text", x=42, y=25.5, label="EV") +
	annotate(geom="text", x=42, y=25.0, label="SPZ") +
	scale_y_continuous(breaks=seq(25,28.6,by=0.4)) +
	scale_x_discrete(breaks=c("2018-06","2018-09","2018-12","2019-03","2019-06","2019-09","2019-12","2020-03","2020-06","2020-09","2020-12","2021-03","2021-06","2021-09","2021-12")) +
	scale_size_continuous("Days detected",range=c(1,10)) +
	scale_color_manual("Depth zone",values = zcol) +
	scale_fill_manual("Depth zone",values = zcol) +
	guides(color = guide_legend(override.aes = list(size=5))) +
	theme_classic() +
	theme(panel.border = element_rect(linetype = "solid", fill = NA),axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = "Latitude")

dd.color
 

#------------------------------------------------------------------------------
