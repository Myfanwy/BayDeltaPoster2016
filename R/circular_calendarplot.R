# Circular bar plot

library(ggplot2)

# data has to look like: group = chn or wst, value = percentage of the year that they're present.

# most white sturgeon present from late October - early April.

# wst
julian(as.Date("2016-10-25"), origin = as.Date("2016-01-01")) #298
julian(as.Date("2016-04-08"), origin = as.Date("2016-01-01")) #98

298-98 # missing for 200 days out of the summer
365.25-298 # 67.25

67.25+98 # present for 165.25 days

165.25/365.25 # present for 45% of the year

#chn
julian(as.Date("2016-01-08"), origin = as.Date("2016-01-01")) #7
julian(as.Date("2016-09-05"), origin = as.Date("2016-01-01")) #98

365.25-248 # present for 117.25 + 7 days = 124.25 days
124.25/365.25

# present for 34% of the year

d <- data.frame(group = c("chn", "wst"), percentage = c(34, 45))

ggplot(d, aes(x = group, y = percentage ,fill = group)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("")  + ylim(c(0, 100))
