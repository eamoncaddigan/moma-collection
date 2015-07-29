# Just curious whether different departments have different acquisition activity
# over the years.

require(dplyr)
require(tidyr)
require(ggplot2)

# Ugh
rm(list=ls())
artworks <- read.csv("Artworks.csv", stringsAsFactors = FALSE)

departments <- c("Painting & Sculpture", 
                 "Drawings", 
                 "Architecture & Design", 
                 "Photography", 
                 "Prints & Illustrated Books")

# Just pull out the year, since dates aren't always formatted correctly, and
# make Department a factor.
artworks <- artworks %>% 
  filter(grepl("[0-9]{4}", DateAcquired)) %>%
  mutate(year_acquired = as.numeric(sub(".*([0-9]{4}).*", "\\1", DateAcquired)),
         Department = ifelse(Department %in% departments, Department, "Other"),
         Department = factor(Department, c("Other", departments)))

# Find the cummulative number of works from each department
artworks.tally <- artworks %>% 
  group_by(year_acquired, Department) %>% 
  tally() %>% 
  group_by(Department) %>% 
  mutate(total_works=cumsum(n))

# XXX - This is buggy here, the notches seem to be a ggplot2 error. :(
p1 <- artworks.tally %>%
  ggplot(aes(x=year_acquired, y=total_works, fill=Department)) + 
  geom_area(position="stack") + 
  theme_minimal() + 
  scale_fill_brewer(type="qual", palette = "Dark2") +
  scale_x_continuous(breaks=round(seq(min(artworks$year_acquired), 
                                      max(artworks$year_acquired), 
                                      length.out = 5))) +
  labs(title = "MoMA's cummulative works by time", 
       y = "Total works",
       x = "Year acquired")
print(p1)
ggsave("cummulative_works.png")
