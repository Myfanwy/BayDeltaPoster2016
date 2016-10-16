# Make a list of tagging groups (to filter detections by)

a <- alltags
head(a)
?split
groups <- split(a, a$`Tagging Group`)
str(groups)
names(groups)

g <- map(groups, select, TagID) # selects only the TagID column of each dataframe
tags <- unlist(g, recursive = FALSE) # have to unlist the first level to be able to manipulate with these as vectors
tags

d <- all69khz_grouped
head(d)

d2 <- filter(d, TagID %in% tags$fca_2012.TagID) # now we have an easy way to filter the detections based upon tagging group

d3 <- filter(d, TagID %in% tags$wst_2012.TagID) # contains all the detections for this tagging group - have to filter further by year

d3 <- d3 %>% 
  filter(DateTimeUTC > "2013-06-01 00:00:00", DateTimeUTC < "2014-05-31 23:59:59")

# Need a way to programmatically separate detections by detection year and tagging group, and save them all as separate dataframes in order to calculate residence time from.

# By hand: except for 2012, detection years start on July 1st ever year, and end on June 30th.
dstart = c("2011-07-01 00:00:00", 
           "2012-07-01 00:00:00", 
           "2013-07-01 00:00:00", 
           "2014-07-01 00:00:00", 
           "2015-07-01 00:00:00", 
           "2016-07-01 00:00:00")

dend <- c("2012-06-30 00:00:00", 
          "2013-06-30 00:00:00", 
          "2013-06-30 00:00:00",
          "2014-06-30 00:00:00",
          "2015-06-30 00:00:00",
          "2016-06-30 00:00:00")

detyears <- data_frame(dstart, dend)
detyears$detyear <- 2011:2016
(detyears)

# Now need to merge detyear column with fishpaths, conditional on dstart and dend
fp <- fishpaths(d, d$TagID, d$Station)

d3comp <- d %>% 
  filter(TagID %in% tags$wst_2012.TagID) 