load("data_tidy/fishpaths11-15.Rdata") #generates object d3; this is just all the Yolo Bypass detections for all tagged fish through fall chn 2015
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

comps <- f %>%  # run the model on this one
  group_by(Sp, detyear) %>% 
  mutate(nfish = len(TagID)) %>% 
  filter(!duplicated(detyear)) %>% 
  select(detyear, Sp, nfish)
comps

esc <- read.csv("data_tidy/escapement.csv", stringsAsFactors = FALSE, header = TRUE)
str(esc)
esc$detyear <- as.numeric(esc$detyear)

esc2 <- reshape2::melt(esc, id.vars = c("detyear", "species", "sample_size"), value.name = "count")
esc2 <- filter(esc2, detyear != 2011)
esc2$species <- factor(esc2$species, labels = c("Chinook salmon", "white sturgeon"))
  
ggplot(esc2, aes(x = detyear, y = count, fill = variable)) + 
  facet_wrap(~species, labeller = label_value) + 
  geom_rect(aes(fill = species), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = FALSE) +
   geom_bar(stat = 'identity', color = "black", width = 0.5, position = "stack") +
  scale_y_continuous(expand = c(0, 0.3), limits = c(0, 55)) +
  scale_fill_manual(values = c("#440154FF" , "white", "gray40", "#FDE725FF")) +
  labs(y = "Count") +
  coord_flip() +
   theme(text = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(15, "points"),
        legend.position = "none")

ggsave(filename = "figures/escapement.jpg", width = 12, height = 5.5)

# Wrap by year instead
ggplot(esc2, aes(x = species, y = count, fill = variable)) + 
  facet_wrap(~detyear, labeller = label_value) + 
  geom_rect(aes(fill = species), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = FALSE) +
  geom_bar(stat = 'identity', color = "black") +
  scale_fill_manual(values = c("#440154FF" , "white", "gray40", "#FDE725FF")) +
  labs(y = "Count") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Try w/ geom_ribbon
ggplot(esc2, aes(x = detyear, y = count, fill = variable)) + 
  geom_ribbon(aes(fill = variable, color = variable, ymin = 0, ymax = count), stat = "identity", alpha = 0.2) +
  facet_wrap(~species, scales = "free_y") +
  labs(y = "Count") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))



