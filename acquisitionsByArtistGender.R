# Just curious whether different departments have different acquisition activity
# over the years.

require(dplyr)
require(tidyr)
require(ggplot2)

# Well that was easy
momaDB <- src_sqlite("momaDB.sqlite")
artworks <- tbl(momaDB, "artworks")
artists <- tbl(momaDB, "artists")


# Add new artist info to the artworks table -------------------------------

artists <- select(artists,
                  Artist, ArtistBio, iso3166, gender)
artworks <- left_join(artworks, artists, by = c("Artist", "ArtistBio"))


# Just pull out the year, since dates aren't always formatted correctly.
artworks <- artworks %>% 
  collect %>%
  filter(grepl("[0-9]{4}", DateAcquired)) %>%
  mutate(year_acquired = as.integer(sub(".*([0-9]{4}).*", "\\1", DateAcquired)),
         gender = ifelse(is.na(gender), "unknown", gender)) %>%
  select(year_acquired, gender)

# Find the cumulative number of works from each department
artworks <- artworks %>% 
  group_by(year_acquired, gender) %>% 
  tally() %>% 
  # Fill in missing years
  spread(gender, n, fill = 0) %>% gather("gender", "n", -year_acquired) %>%
  # Find the total works over time
  group_by(gender) %>% 
  mutate(total_works=cumsum(n)) %>%
  ungroup() %>%
  mutate(gender = factor(gender, c("unknown", "female", "male")))

p1 <- artworks %>%
  ggplot(aes(x=year_acquired, y=total_works, fill=gender, order=gender)) + 
  geom_area(position="stack") + 
  theme_minimal() + 
  scale_fill_manual(values=c("#4daf4a", "#e41a1c", "#377eb8"),
                    guide = guide_legend(title = "Artist Gender")) +
  scale_x_continuous(breaks=round(seq(min(artworks$year_acquired), 
                                      max(artworks$year_acquired), 
                                      length.out = 5))) +
  labs(title = "MoMA's cumulative works by time", 
       y = "Total works",
       x = "Year acquired")
print(p1)
ggsave("cumulative_works_by_gender.png")

p2 <- artworks %>%
  filter(gender != "unknown")%>%
  ggplot(aes(x=year_acquired, y=n, fill=gender, color=gender)) + 
  geom_area(position="identity", alpha=0.1) + 
  geom_line(size=1.2) + 
  theme_minimal() + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    guide=FALSE) +
  scale_color_manual(values=c("#e41a1c", "#377eb8"),
                    guide = guide_legend(title = "Artist Gender")) +
  scale_x_continuous(breaks=round(seq(min(artworks$year_acquired), 
                                      max(artworks$year_acquired), 
                                      length.out = 5))) +
  labs(title = "MoMA's acquisitions by year", 
       y = "Number of acquisitions",
       x = "Year acquired")
print(p2)
ggsave("total_works_by_gender.png")

p3 <- artworks %>%
  filter(gender != "unknown") %>%
  group_by(year_acquired) %>%
  mutate(percent_n = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=year_acquired, y=percent_n, fill=gender, order=gender)) + 
  geom_area(position="stack") + 
  theme_minimal() + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    guide = guide_legend(title = "Artist Gender")) +
  scale_x_continuous(breaks=round(seq(min(artworks$year_acquired), 
                                      max(artworks$year_acquired), 
                                      length.out = 5))) +
  labs(title = "MoMA's acquisitions and artist gender", 
       y = "Percentage of works acquired",
       x = "Year acquired")
print(p3)
ggsave("percent_works_by_gender.png")
