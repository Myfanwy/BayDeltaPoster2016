library(tidyverse)

# First try with one fish.
test <- filter(d3, TagID == 2841)
head(test)

# calculate the transit times for each movement, even the useless ones
test$transittime <- "NA"
for(i in 1:length(test$departure)) {
  test$transittime[i] =  difftime(test$arrival[i+1], test$departure[i], units = "hours")
}

test$transittime <- as.numeric(test$transittime) # transittime is now still in hours, but numeric so that I can convert it to a rate later on
head(select(test, TagID, Station, arrival, departure, transittime))

# We can see that this fish goes from Rstr_joint to BC_joint and back to Rstr_joint, but there are some encounters in there at BC_joint that represent a movement distance of 0.  I don't want those movements included in transit rate calculation, so I create a unique ID for each meaningful movement in order to filter them out later:

test$move_num <- cumsum(!duplicated(test$Station) | c(F, abs(diff(test$Rkm)) > 0)) # creates a grouping ID for every time a fish changes stations (i.e. the difference in riverkm is > 0)

# the result is:
head(select(test, TagID, Station, arrival, departure, transittime, move_num))

# Now that I have them grouped, I need to grab the right rows: if there's only one move_num ID, give me that one.  If there are multiples of the same group number, give me the last of those, since that's the row that precedes a meaningful movement.  I can do this with dplyr and slice(length()):

test2 <- test %>% 
  group_by(move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()

# And the result:
head(select(test2, TagID, Station, arrival, departure, move_num)) 

# for-loop to calculate difference in river kilometer, in preparation for calculating a rate:
test2$diffRkm <- "NA"
for (i in 1:length(test2$Rkm)) {
  test2$diffRkm[i] = abs(test2$Rkm[i+1] - test2$Rkm[i])
}

# finally, calculate the movement rate as a new column:
test2$rate <- test2$transittime/as.numeric(test2$diffRkm)
mean(test2$rate, na.rm = TRUE) 


# Checking wiht one fish again:

c1 <- filter(dp, TagID == 20157)

c1 <- c1 %>% 
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  mutate(transit_time = difftime(lead(arrival), departure, units = "hours")) %>% 
  ungroup()

range(c1$transit_time, na.rm = TRUE)
c1$transit_time <- as.numeric(c1$transit_time)
range(c1$transit_time, na.rm = TRUE)

# Filtering criteria: need to exclude residence == 0
# also getting movement rates for when sturgeon come back the following year, which is often more than 120 days.  Need to filter out those as well.
# getting some simultaneous detections between BC_joint and BC_joint2... filter out those with the grouping distances, I think

# re: the negative detections: they seem to between very close stations.  My thought is that it comes from the time threshold in fishpaths() function - maybe I'll try tightening that up and see if it helps.  For now, just filter out any transit times taht are less than 0.

max <- dp %>% 
  arrange(-transit_time) %>% 
  select(TagID, Station, arrival, departure, transit_time, Rkm)
head(max) # there are some crazy values here - need to go back and spot-check with individual fish.


# make grouping ids and slice rows by them
dp <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  mutate(move_num = cumsum(!duplicated(Station) | c(F, abs(diff(Rkm)) > 0))) %>% 
  ungroup() %>% 
  group_by(detyear, TagID, move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()

dp2 <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(move_num) %>% 
  mutate(diffRkm = abs(lead(Rkm) - Rkm),
         rate = diffRkm/transit_time) %>% 
  ungroup

dp3 <- dp2 %>% 
  group_by(detyear, TagID) %>% 
  summarise(meanrate = mean(rate, na.rm = TRUE)) 

range(dp3$meanrate, na.rm = TRUE)

ggplot(dp2, aes(x = Sp, y = meanrate)) + 
  geom_boxplot(alpha = 0.6) + 
  geom_jitter(aes(color = Sp), size = 3, width= 0.8,    alpha = 0.5)
