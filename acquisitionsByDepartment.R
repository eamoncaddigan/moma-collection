# Just curious whether different departments have different acquisition activity
# over the years.

require(dplyr)
require(tidyr)
require(ggplot2)

# Ugh
rm(list=ls())
artworks <- read.csv("Artworks.csv", stringsAsFactors = FALSE)

# Just pull out the year, since dates aren't always formatted correctly, and
# make Department a factor.
artworks <- artworks %>% 
  filter(grepl("[0-9]{4}", DateAcquired)) %>%
  mutate(year_acquired = as.numeric(sub(".*([0-9]{4}).*", "\\1", DateAcquired)),
         Department = factor(Department, c("Painting & Sculpture", 
                                           "Drawings", 
                                           "Architecture & Design", 
                                           "Photography", 
                                           "Prints & Illustrated Books"))) %>%
  filter(!is.na(Department))

# Find the cummulative number of works from each department
artworks.tally <- artworks %>% 
  group_by(year_acquired, Department) %>% 
  tally() %>% 
  group_by(Department) %>% 
  mutate(total_works=cumsum(n))

# This is buggy here. Gonna try to run it somewhere else. :/
p1 <- ggplot(artworks.tally, aes(x=year_acquired, y=total_works, fill=Department)) + 
  geom_area(position="stack") + 
  theme_minimal() + 
  scale_fill_brewer(type="qual", palette = "Dark2")
print(p1)
#ggsave("age_at_acquisition.png")
