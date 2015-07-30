# After running grabGenderData, there are a bunch of common (worldwide) names
# that have no gender estimate. This picks those out and appends them to the
# list of names to genderize with the country stripped out so that they may be
# queried generally.

artistData <- read.csv("names_to_genderize.csv", stringsAsFactors = FALSE)
artistData.noCountry <- filter(artistData, iso3166 == "none")

# Okay. NOW, go through and look up the names that have countries but didn't get
# results, this time stripping away the country.
namesWithGenders <- read.csv("names_with_genders.csv", stringsAsFactors = FALSE)
namesWithGenders.missing <- namesWithGenders %>% 
  filter(is.na(gender), country_id != "none") %>%
  select(name) %>% 
  distinct()

print(nrow(namesWithGenders.missing))
namesWithGenders.missing <- anti_join(namesWithGenders.missing, artistData.noCountry,
                                      by = c("name" = "first_name"))
print(nrow(namesWithGenders.missing))

# Match the format of artistData
namesWithGenders.missing <- namesWithGenders.missing %>%
  mutate(iso3166 = "none") %>%
  select(first_name = name, iso3166)

write.csv(namesWithGenders.missing, "names_to_genderize_2.csv", 
          row.names = FALSE)
