# Transit Speed
library(tidyverse)
library(ybp)
library(fishtrackr)
library(lubridate)
library(viridis)
library(beepr)

d <- all69khz_grouped
# add detyear column
d2 <- d %>% 
  arrange(DateTimeUTC) %>% 
  mutate(detyear = 
           ifelse(
             DateTimeUTC > "2011-07-01 00:00:00" & DateTimeUTC < "2012-06-30 00:00:00", "2011",
             ifelse(
               DateTimeUTC > "2012-07-01 00:00:00" & DateTimeUTC < "2013-06-30 00:00:00", "2012",
               ifelse(
                 DateTimeUTC > "2013-07-01 00:00:00" & .$DateTimeUTC < "2014-06-30 00:00:00", "2013",
                 ifelse(
                   DateTimeUTC > "2014-07-01 00:00:00" & DateTimeUTC < "2015-06-30 00:00:00", "2014", "2015"
                 )
               )
             )
           ) )

# filter for false dets
d2 <- d %>% 
  group_by(TagID) %>% 
  filter(DateTimeUTC >= DateTagged) %>% 
  arrange(DateTimeUTC)

# filter for shed tags

d3 <- fishpaths(d2, d2$TagID, d2$Station)
beep()
d3$residence <- d3$departure - d3$arrival
units(d3$residence) = "days"
sheds <- filter(d3, residence > 7) # filter out encounters equal to or longer than 7 consecutive days

len(sheds$TagID)

# Now filter out those long-res IDs:
d3 <- filter(d2, !(TagID %in% sheds$TagID))
len(d3$TagID) # should be 221-6 = 215

# now call fishpaths on d3, add detyear later:
d3 <- fishpaths(d3, d3$TagID, d3$Station)
beep(2)
d3 <- d3 %>% 
  arrange(arrival) %>% 
  mutate(detyear = 
           ifelse(
             departure > "2011-07-01 00:00:00" & departure < "2012-06-30 00:00:00", "2011",
             ifelse(
               departure > "2012-07-01 00:00:00" & departure < "2013-06-30 00:00:00", "2012",
               ifelse(
                 departure > "2013-07-01 00:00:00" & .$departure < "2014-06-30 00:00:00", "2013",
                 ifelse(
                   departure > "2014-07-01 00:00:00" & departure < "2015-06-30 00:00:00", "2014", "2015"
                 )
               )
             )
           ) )
head(select(d3, arrival, departure, detyear))
tail(select(d3, arrival, departure, detyear))

# Calculate transit time

d3 <- d3 %>% 
   group_by(detyear, TagID) %>% 
   arrange(arrival) %>% 
   mutate(residence = departure - arrival) %>% 
   ungroup()
units(d3$residence) <- "hours"
head(d3)

# Need to go row-by-row on times, and calculate the transit time in km/hr between the previous station and the current one.

# First try with one fish.
test <- filter(d3, TagID == 2841)
test$transittime <- "NA"

for(i in 1:length(test$departure)) {
test$transittime[i] = 
  difftime(test$arrival[i+1], test$departure[i], units = "hours")
}

str(test)
test$transittime <- as.numeric(test$transittime) # transittime is now in hours, but numeric.

test$move_num <- cumsum(!duplicated(test$Station) | c(F, abs(diff(test$Rkm)) > 0)) # for each unique Station, create a unique grouping variable if the following row has a difference in river kilometer (in other words, create a unique ID for each meaningful movement)

# Now need to grab them by: if there's only one move_num ID, give me that one.  If there are multiples of the same number, give me the last of those multiples.

test2 <- test %>% 
  group_by(move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()

test2$rate <- "NA"
test2$diffRkm <- NULL
for (i in 1:length(test2$Rkm)) {
  test2$diffRkm[i] = abs(test2$Rkm[i+1] - test2$Rkm[i])
}

test2$rate <- test2$transittime/test2$diffRkm
mean(test2$rate, na.rm = TRUE) # a little less than 1km/hr.  Cool.  Let's scale up.



## With All The Fish:
## Dplyr solution:

dp <- d3 %>% 
  group_by(detyear, TagID) %>% 
  mutate(transit_time = difftime(lead(arrival), departure, units = "hours"))

chk <- filter(dp, TagID == 2841)
str(chk)
chk$transit_time <- as.numeric(chk$transit_time)

chk$move_num <- cumsum(!duplicated(chk$Station) | c(F, abs(diff(chk$Rkm)) > 0)) # creates a grouping ID for every time a fish changes stations (i.e. the difference in riverkm is > 0)
 chk2 <- chk %>% 
  group_by(move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()

# And the result:
head(select(chk2, TagID, Station, arrival, departure, move_num)) 
# for-loop to calculate difference in river kilometer, in preparation for calculating a rate:

chk3 <- chk2 %>% 
  group_by(TagID) %>% 
  mutate(diffRkm = abs(lead(Rkm) - Rkm))

chk3 <- select(chk3, TagID, Station,arrival, departure, Rkm, diffRkm, transit_time)

chk3 <- chk3 %>% 
  mutate(rate = diffRkm/transit_time)
mean(chk3$rate, na.rm = TRUE)


## Try on whole df:
dp <- d3 %>% 
  filter(residence != 0) %>% # filter out single detections
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  mutate(transit_time = difftime(lead(arrival), departure, units = "hours")) %>% 
  ungroup()
range(dp$transit_time, na.rm = TRUE)
dp$transit_time <- as.numeric(dp$transit_time)
range(dp$transit_time, na.rm = TRUE)

max <- dp %>% 
  arrange(transit_time) %>% 
  select(TagID, Station, arrival, departure, transit_time, Rkm)
head(max) # there are some crazy values here - need to go back and spot-check with individual fish.

# Filtering criteria: remove those transit_times that are greater than 50 days.

# make grouping ids and slice rows by them
dp <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  filter(transit_time < 50*24, transit_time > 0) %>% # filters out transits greater than 50 days
  mutate(move_num = cumsum(!duplicated(Station) | c(F, abs(diff(Rkm)) > 0.60))) %>% # skip the BC transits
  ungroup() %>% 
  group_by(detyear, TagID, move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()
range(dp$transit_time) # we still have negative transit times, even after filtering for BC_joints - let's see what that's about:

dp2 <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(move_num) %>% 
  mutate(diffRkm = abs(lead(Rkm) - Rkm),
         rate = diffRkm/transit_time) %>% 
  ungroup %>% 
  filter(rate < 100)
range(dp2$rate, na.rm = TRUE)

dp3 <- dp2 %>% 
  group_by(detyear, TagID) %>% 
  mutate(meanrate = mean(rate, na.rm = TRUE)) 

range(dp3$meanrate, na.rm = TRUE)

ggplot(dp2, aes(x = Sp, y = rate)) + 
  geom_boxplot(alpha = 0.6) + 
  geom_jitter(aes(color = Sp), size = 2, width= 0.8,    alpha = 0.1) + facet_wrap(~detyear)

ggplot(dp2) +
  geom_density(aes(x = rate, color = Sp)) + facet_wrap(~detyear, scales = "free_x")

# begin modeling 
library(rethinking)  
d1 <- filter(dp3, detyear == "2013")
d1$dSp <- ifelse(d1$Sp == "chn", 1, 0)
d1 <- as.data.frame(d1)


m <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  
  mu <- a + bSp*dSp,
  
  a ~ runif(0, 20) ,
  
  bSp ~ dnorm(0, 5),
  
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1 )

precis(m) # shows an increase of 0.53km/hour for chinook than for white sturgeon.  But that's just in 2013, when there were many more chn than white sturgeon.

d1all <- as.data.frame(dp3)
d1all <- dplyr::filter(d1all, transit_time > 0)
d1all$dSp <- ifelse(d1all$Sp == "chn", 1, 0)

m1a <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a + bSp*dSp,
  a ~ dnorm(0, 10) ,
  bSp ~ dnorm(0, 1),
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1all )
precis(m1a) # holy shit - when you take all the data into account, being a chn corresponds to a 0.89km/hr increase.


