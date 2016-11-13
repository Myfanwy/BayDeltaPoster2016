d <- all69khz_grouped

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
d2 <- d2 %>% 
  group_by(TagID) %>% 
  filter(DateTimeUTC >= DateTagged) %>% 
  arrange(DateTimeUTC)

# Find max rkm reached for each fish

maxrkms <- d2 %>% 
  group_by(detyear, TagID) %>% 
  mutate(maxrkm = max(Rkm)) %>% 
  ungroup()

head(maxrkms)

maxrkms$detyear <- factor(maxrkms$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))
maxrkms <- filter(maxrkms, detyear != "2011 - 2012")

maxsum <- maxrkms %>% 
  group_by(detyear, Sp, TagID) %>% 
  filter(!duplicated(TagID)) %>% 
  summarise(maxrkm_reached = maxrkm)

rkmplot <- ggplot(maxsum, aes(x = Sp, y = maxrkm_reached)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Sp), size = 3, width= 0.5,    alpha = 0.65) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "River Kilometer", title = "Maximum River Kilometer Reached")

rkmplot + theme(text = element_text(size = 18),
                axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.position = "none")

ggsave(filename = "figures/maxrkmplot.jpg", width = 8, height = 5, units = "in")
