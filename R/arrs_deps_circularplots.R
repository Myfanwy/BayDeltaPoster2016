#load("../data_tidy/fishpaths11-15.Rdata") #generates object d3; this is just all the Yolo Bypass detections for all tagged fish through fall chn 2015
library(lubridate)
## filter out tag day detections #-------------
d3 <- all69khz_grouped
head(d3)
d3$DateTagged_hms <- "01:00:00"
d3$Datepaste <- format(as.POSIXct(paste(d3$DateTagged, d3$DateTagged_hms)), "%Y-%m-%d %H:%M:%S")
d3$DateTagged_full <- ymd_hms(d3$Datepaste)
d3$DateTagged <- d3$DateTagged_full

# filter out the detections that happened the day they were tagged
f2 <- d3 %>% 
  group_by(TagID) %>%
  mutate(tagdetthreshold = DateTagged + 60*60*24) %>% 
  filter(DateTimeUTC > tagdetthreshold) #3600 less detections
#----------------------------------------------------
f <- select(f2, -DateTagged_full, -Datepaste, -DateTagged_hms, -tagdetthreshold) # get back to the columns we want
head(f)
# Can now run fishpaths function
f <- fishpaths(f, f$TagID, f$Station)
# fix time zones of arrival and departure columns
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
f$arrival <- format(f$arrival, tz="Pacific/Pitcairn", usetz=TRUE)
f$departure <- format(f$departure, tz="Pacific/Pitcairn", usetz=TRUE)
f$hour_arr <- hour(f$arrival)
f$hour_dep <- hour(f$departure)

f$detyear <- factor(f$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))

chn_deps <- f %>% 
  filter(Sp == "chn") %>% 
  group_by(detyear, hour_dep, Station) %>% 
  tally()

chn_arrs <- f %>% 
  filter(Sp == "chn") %>% 
  group_by(detyear, hour_arr, Station) %>% 
  tally()


chn_arrivals <- ggplot(chn_arrs, aes(x = hour_arr, y = n)) + 
  geom_bar(stat = "identity", fill = "#440154FF") + 
  coord_polar(start = 0) + 
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

chn_arrivals + theme(text = element_text(size = 18),
                     plot.title = element_text(hjust = 0.5),
                     legend.position = "none")


chn_departures <- ggplot(chn_deps, aes(x = hour_dep, y = n)) + 
  geom_bar(stat = "identity", fill = "#440154FF") + 
  coord_polar(start = 0) +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

chn_departures + theme(text = element_text(size = 18),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "none")


# wst ----------------------------------
wst_deps <- f %>% 
  filter(Sp == "wst", detyear != "2011 - 2012") %>% 
  group_by(detyear, hour_dep, Station) %>% 
  tally()

wst_arrs <- f %>% 
  filter(Sp == "wst", detyear != "2011 - 2012") %>% 
  group_by(detyear, hour_arr, Station) %>% 
  tally()


wst_arrivals <- ggplot(wst_arrs, aes(x = hour_arr, y = n)) + 
  geom_bar(stat = "identity", fill = "#FDE725FF") + 
  coord_polar(start = 0) + 
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

wst_arrivals + theme(text = element_text(size = 18),
                     plot.title = element_text(hjust = 0.5),
                     legend.position = "none")


wst_departures <- ggplot(wst_deps, aes(x = hour_dep, y = n)) + 
  geom_bar(stat = "identity", fill = "#FDE725FF") + 
  coord_polar(start = 0) +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

wst_departures + theme(text = element_text(size = 18),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "none")

# do white sturgeon just have more arrivals/departures?
head(f)
comps <- f %>%  # run the model on this one
  group_by(Sp, detyear) %>% 
  mutate(nfish = len(TagID)) %>% 
  filter(!duplicated(detyear)) %>% 
  select(detyear, Sp, nfish)
comps

dets <- f %>% 
  count(Sp, detyear, TagID) %>% 
  group_by(detyear, Sp) %>% 
  summarise(meanarrs = mean(n), sdarrs = sd(n))
  
head(dets)  

summarise(count = n())
