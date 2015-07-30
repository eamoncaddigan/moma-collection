# A request! https://twitter.com/mkurlandsky/status/626363245675286528

require(dplyr)
require(tidyr)
require(ggplot2)

# Read the artist information out of the collection table -----------------

artists <- read.csv("Artworks.csv", stringsAsFactors = FALSE) %>%
  select(Artist, ArtistBio) %>%
  distinct() %>%
  # Remove entries with missing bios
  filter(ArtistBio != "",
         !grepl("unknown", ArtistBio, ignore.case = TRUE),
         grepl("[[:alpha:]]+", ArtistBio),
         !grepl("unknown", Artist, ignore.case = TRUE)) %>%
  # Try to remove anything with multiple artists
  filter(!grepl(" and ", Artist), 
         !grepl(",", Artist))


# Extract birth nation and nationality from the artist bio ----------------

artists <- artists %>%   
  mutate(ArtistBio = sub("U\\.S\\.A\\.", "United States", ArtistBio),
         birth_nation = ifelse(grepl("born [[:alpha:]]+", ArtistBio),
                               sub(".*born ([[:alpha:][:space:]]*).*", "\\1", ArtistBio),
                               NA),
         birth_nation = ifelse(birth_nation %in% c("c", "ca"), NA, birth_nation),
         birth_nation = sub("\\W*$", "", birth_nation),
         birth_nation = ifelse(birth_nation %in% c("US", "USA"), "United States", birth_nation),
         nationality = sub("^[^[:alpha:]]([[:alpha:][:space:]]*).*", "\\1", ArtistBio),
         nationality = sub("\\W*$", "", nationality),
         nationality = sub(" and .*", "", nationality),
         first_name = sub("([[:alpha:]]*).*", "\\1", Artist))


# Sort out the country and language info (we only lose a few artists) -----

setwd("countries")
source("combineCountryInfo.R")
setwd("..")
artists <- artists %>%
  # Use a join to convert nationalities to countries
  left_join(nationalityToCountry, by = "nationality") %>%
  # If the artist had a stated birth nation, use that
  mutate(country = ifelse(!is.na(birth_nation), birth_nation, country)) %>%
  # Add the language info
  left_join(countryToLanguage, by = "country") %>%
  # Get rid of anybody who slipped through the cracks
  filter(nationality != "", !is.na(iso639), !is.na(country))

# Find unique first name / (probably) language pairs for genderizing
artist.firstNames <- artists %>%
  select(first_name, iso639) %>%
  distinct()
print(nrow(artist.firstNames))
write.csv(artist.firstNames, "names_to_genderize.csv", row.names = FALSE)
