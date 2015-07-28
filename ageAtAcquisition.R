require(dplyr)
require(tidyr)
require(ggplot2)

artworks <- read.csv("Artworks.csv", stringsAsFactors = FALSE)

artworks <- artworks %>% 
  filter(grepl("[0-9]{4}", DateAcquired),
         grepl("[0-9]{4}", Date)) %>%
  mutate(year_acquired = as.numeric(sub(".*([0-9]{4}).*", "\\1", DateAcquired)),
         year_started =  as.numeric(sub(".*([0-9]{4}).*", "\\1", Date)),
         acquisition_age = year_acquired-year_started) %>%
  filter(acquisition_age >= 0)

ggplot(artworks, aes(x=year_acquired, y=acquisition_age)) + 
  geom_point(alpha = 0.01, position = position_jitter(w = 0.5, h = 0.5)) + 
  geom_smooth(se=FALSE, size=1.5) + 
  theme_minimal() + 
  scale_y_continuous(limits=c(0, 200)) +
  scale_x_continuous(breaks=round(seq(min(artworks$year_acquired), 
                                      max(artworks$year_acquired), 
                                      length.out = 5))) +
  labs(title="MoMA keeps things fresh", 
       y="Age (years) at time of acquisition", 
       x="Year acquired")
ggsave("age_at_acquisition.png")
