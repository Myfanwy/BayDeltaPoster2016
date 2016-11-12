load("../data_tidy/fishpaths11-15.Rdata") #generates object d3; this is just all the Yolo Bypass detections for all tagged fish through fall chn 2015
library(lubridate)
## filter out tag day detections #-------------
# d3 <- all69khz_grouped

f <- d3
f <- f %>% 
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

# don't run until you're ready to plot:
f$detyear <- factor(f$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))

head(f)

fl <- split(f, as.factor(f$detyear))


f1 <- f %>% 
 # add 1 chn to 2015, when BC_joint picked up > BC2
  group_by(Sp, detyear, TagID) %>% 
  arrange(DateTimeUTC) %>%
  filter(!duplicated(DateTimeUTC)) %>%
  summarise(first = paste(Station[DateTimeUTC == min(DateTimeUTC)]),
            last = paste(Station[DateTimeUTC == max(DateTimeUTC)])) %>% 
  ungroup()

lasts <- f1 %>% 
  group_by(Sp, detyear, TagID) %>% 
  filter(last == "BC_joint" | last == "BC_joint2" | last == "Base_TD") %>% 
  ungroup %>% 
  group_by(detyear, Sp) %>% 
   summarise( n = n())
lasts

## Getting closer - not sure how to deal with tagids that are detected both BC_joint/2.  
  
summ_last <- left_join(comps, lasts)
summ_last

# Table of the number of fish per species each year (use to join later)
comps <- f %>%  # run the model on this one
  group_by(Sp, detyear) %>% 
  mutate(nfish = len(TagID)) %>% 
  filter(!duplicated(detyear)) %>% 
  select(detyear, Sp, nfish)
comps
