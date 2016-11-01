# Make a list of tagging groups (to filter detections by)

a <- alltags
head(a)
groups <- split(a, a$`Tagging Group`)
names(groups)

g <- map(groups, select, TagID) # selects only the TagID column of each dataframe
tags <- unlist(g, recursive = FALSE) # have to unlist the first level to be able to manipulate with these as vectors
tags

d <- all69khz_grouped
head(d)

d2 <- filter(d, TagID %in% tags$fca_2012.TagID) # now we have an easy way to filter the detections based upon tagging group

d3 <- filter(d, TagID %in% tags$wst_2012.TagID) # contains all the detections for this tagging group - have to filter further by year

wst12 <- d3 %>% 
  filter(DateTimeUTC > "2011-06-01 00:00:00", DateTimeUTC < '2012-05-31 23:59:59') # keep in mind that this is all in UTC still
fp_wst12 <- fishpaths(wst12, wst12$TagID, wst12$Station)

# calculate residence - raw monitor residence
fp_wst12$residence <- fp_wst12$departure - fp_wst12$arrival
units(fp_wst12$residence) <- "days"

firstlast_wst12 <- fp_wst12 %>% 
  group_by(TagID) %>% 
  arrange(arrival, departure) %>% 
  slice(c(1, length(departure))) # slices the row of the earliest arrival and the latest departure

meanres_wst12 <- firstlast_wst12 %>% 
  group_by(TagID) %>% 
  mutate(totalres = departure[2] - arrival[1]) %>% 
  ungroup() %>%
  summarise(meanres = mean(totalres), sdres = sd(totalres), n = len(TagID)) # need to properly assign year(s) to these data

# combining those 2 chains above:

meanres_wst12_combinedchain <- 
fp_wst12 %>% 
  group_by(TagID) %>% 
  arrange(arrival, departure) %>% 
  slice(c(1, length(departure))) %>% 
  mutate(totalres = departure[2] - arrival[1]) %>% 
  ungroup() %>%
  summarise(meanres = mean(totalres), sdres = sd(totalres), n = len(TagID)) # works; move to new script to complete calculations


wst13 <- d3 %>% 
  filter(DateTimeUTC > "2013-06-01 00:00:00", DateTimeUTC < "2014-05-31 23:59:59")

fp_wst13 <- fishpaths(wst13, wst13$TagID, wst13$Station)
head(fp_wst13)

firstlast_wst13 <- fp_wst13 %>% 
  group_by(TagID) %>% 
  arrange(arrival, departure) %>% 
  slice(c(1, length(departure))) # slices the row of the earliest arrival and the latest departure

meanres_wst13 <- firstlast_wst13 %>% 
  group_by(TagID) %>% 
  mutate(totalres = departure[2] - arrival[1]) %>% 
  ungroup() %>%
  summarise(meanres = mean(totalres), sdres = sd(totalres), n = len(TagID)) # need to properly assign year(s) to these data


# For now it's all copy and paste; Need a way to programmatically separate detections by detection year and tagging group, and save them all as separate dataframes in order to calculate residence time from.




# By hand: except for 2012, detection years start on July 1st every year, and end on June 30th.
dstart = c("2011-07-01 00:00:00", 
           "2012-07-01 00:00:00", 
           "2013-07-01 00:00:00", 
           "2014-07-01 00:00:00", 
           "2015-07-01 00:00:00", 
           "2016-07-01 00:00:00")

dend <- c("2012-06-30 00:00:00", 
          "2013-06-30 00:00:00", 
          "2014-06-30 00:00:00",
          "2015-06-30 00:00:00",
          "2016-06-30 00:00:00",
          "2017-06-30 00:00:00")

detyears <- data_frame(dstart, dend)
detyears$detyear <- 2011:2016
(detyears)

# Adding detyear to the big dataframe: first proof of concept, then re-run with whole set:
d <- all69khz_grouped
d <- sample_n(d, 10, replace = FALSE)
d <- arrange(d, DateTimeUTC)

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
      )
        ) # worked!

# did that break fishpaths?
fp <- fishpaths(d2, d2$TagID, d2$Station)
# yes, it did - could either run the same thing on fishpaths, or add detyear to the function.  will add as an issue to ybp.

d3comp <- d %>% 
  filter(TagID %in% tags$wst_2012.TagID) 