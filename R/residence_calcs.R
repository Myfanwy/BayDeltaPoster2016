#  Residence Time #
library(ybp)
library(fishtrackr)
library(dplyr)
library(lubridate)
# Exploring simultaneous detections ---------------------------------------

f <- all69khz
f$year <- year(f$DateTagged)

f <- filter(f, year == 2012)
f <- filter(f, TagID == 2844)
f <- select(f, -year)

f2 <- fishpaths(f, f$TagID, f$Station)
head(f2)

# do same, but filter duplicate detections first
fsdups <- filter(f, !duplicated(DateTimeUTC))
f3 <- fishpaths(fsdups, fsdups$TagID, fsdups$Station)

identical(f2, f3)

f2$residence = f2$departure - f2$arrival
f3$residence = f3$departure - f3$arrival
units(f2$residence) <- "days"
units(f3$residence) <- "days"

f2$res_runsum = cumsum(as.numeric(f2$residence))
f3$res_runsum = cumsum(as.numeric(f3$residence))


f2 <- f2 %>% 
  group_by(TagID) %>% 
  arrange(TagID, arrival) %>% 
  filter(Sp == "wst")

w2 <- filter(f, TagID == 2844)
head(w2)
w2 <- filter(w2, Station == 'BCE' | Station == "BCW")
w2 <- arrange(w2, DateTimeUTC)
w2$DateTimeUTC[390] - w2$DateTimeUTC[1] # detections span 23.4 days

## Find simultaneous detections and count them
w2dups <- w2[duplicated(w2$DateTimeUTC), ] # this pulled all the BCE detections - there are 60 matching BCW detections to these
duppaths <- fishpaths(w2dups, w2dups$TagID, w2dups$Station)
duppaths$residence = duppaths$departure - duppaths$arrival

# First calculate total residence time with the duplicate detections

head(w2)
w2paths <- fishpaths(w2, w2$TagID, w2$Station)
head(w2paths)
w2paths$residence = w2paths$departure - w2paths$arrival
total = sum(w2paths$residence)
total # total monitor residence of 707 minutes, or 0.49 days

# and without
w22 <- w2[!duplicated(w2$DateTimeUTC), ]
w22paths <- fishpaths(w22, w22$TagID, w22$Station)
w22paths$residence = w22paths$departure - w22paths$arrival
total_sansdups = sum(w22paths$residence)
units(total_sansdups) <- "days"
total_sansdups  # total monitor residence of 0.48 days

total - total_sansdups
# why on earth would filtering duplicate detections lead to a difference of 13.5 minutes??


## From fishpaths dataframe: want to filter for duplicate arrival times.  If there are duplicates, look in the departure column for the same row.  Keep the duplicate arrival that has a later departure time, and discard the duplicate that has an earlier departure time.

f <- all69khz
f$year <- year(f$DateTagged)

f <- filter(f, year == 2012)
f <- filter(f, TagID == 2844)
f <- select(f, -year)

f2 <- fishpaths(f, f$TagID, f$Station)
head(f2) # this dataframe contains duplicate arrival times.

# This code groups the dataframe by arrival (so if there are any duplicates, it will be grouped by them) and slices out the rows that have the maximum departure
maxdups <- f2 %>% 
  group_by(arrival) %>% 
  slice(which.max(departure))

# filtering out the encounters at the gate that are contained within other encounters, or at least merging/selecting them:

# group the receivers first (BC_joint, BC2_joint)

f <- all69khz_grouped
f$year <- year(f$DateTagged)

f <- filter(f, year == 2012)
f <- filter(f, TagID == 2844)
f <- select(f, -year)

f2 <- fishpaths(f, f$TagID, f$Station)
head(f2) # this dataframe doesn't contain duplicate arrival times, because the pre-grouping took care of them

# One method of removing overlapping periods, if it comes to that:
rm_overlaps <- f2 %>% 
  group_by(TagID) %>% 
  arrange(arrival, departure) %>% 
  mutate(indx = c(0, cumsum(as.numeric(lead(arrival)) >
                             cummax(as.numeric(departure)))[-n()] )) %>% 
  group_by(TagID, indx) %>% 
  summarise(arrival = first(arrival), departure = last(departure))

#  Now calculate residence based upon first arrival in the Toe DRain and final departure in the toe drain:
library(beepr)
f <- all69khz_grouped
f2 <- fishpaths(f, f$TagID, f$Station)
beep(7)

firstlast <- f2 %>% 
  mutate(year = year(DateTagged)) %>% 
  group_by(year, TagID) %>% 
  arrange(arrival, departure) %>% 
  slice(c(1, length(departure)))  # assuming that worked, can now group by TagID and calculate total residence

# Summarizing total residence by species and Tagging Season
meanres <- firstlast %>% 
  group_by(TagID, year) %>% 
  mutate(totalres = departure[2] - arrival[1], avgres = mean(totalres)) %>% 
  ungroup() %>% 
  group_by(Sp, year) %>% 
  summarise(meanres = mean(totalres), sdres = sd(totalres)) # need to properly assign year(s) to these data

## Tagging seasons:

# 2012: March - December
# 2013: September - December
# 2014: March - December
# 2015: September - December

# Need to assign a column for TaggingYear, then modify the fishtrackr functions.  Ideally I should assign this column in all69khz and all69khz_grouped.

f %>% 
  ifelse()

meanres

#  Relative Residence calculation plan:
# Get final detection points for each fish
# in 2012, RSTR or Base_TD is an acceptable final detection point for exiting the Bypass
# for 2013 on, must be BCE/W/2

sumary <- f %>% 
  arrange(DateTimeUTC) %>% 
  group_by(TagID)  %>% 
  slice(c(1, length(DateTimeUTC)))
  
firstlast <- sumary %>% 
  group_by(TagID) %>% 
  mutate(first = Station[1], last = Station[2]) %>% 
  filter(!duplicated(TagID)) %>% 
  select(TagID, Sp, year, first, last)
head(firstlast)

firstlast %>% 
  group_by(year, Sp, last) %>% 
  summarise(count = n())

# now that we have final detection locations, need to filter the fish that "exited" the Bypass vs. those that did not, then calculate residence times.






  mutate(first = test$Station[test$DateTimeUTC == min(test$DateTimeUTC)], last = test$Station[test$DateTimeUTC == max(test$DateTimeUTC)]) # that works; so for some reason it's not actually grouping by the TagID when you give it the full dataset

head(sumary) 

sumary <- f %>% 
  arrange_(f$DateTimeUTC) %>% 
  group_by_(f$TagID)  %>% 
  mutate_(f$first = f$Station[f$DateTimeUTC == min(f$DateTimeUTC)], f$last = f$Station[f$DateTimeUTC == max(f$DateTimeUTC)]) 

sumary <- tapply(f$DateTimeUTC, f$TagID, FUN=min)

