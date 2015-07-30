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
         nationality = ifelse(!is.na(birth_nation), birth_nation, nationality),
         first_name = sub("([[:alpha:]]*).*", "\\1", Artist))


# Add country codes to each artist ----------------------------------------

nationalitiesToCodes <- read.csv("countries/nationalities_codes.csv", stringsAsFactors = FALSE)
artists <- artists %>%
  # Add the country code if there is one
  left_join(nationalitiesToCodes, by = "nationality")

# Find unique first name / country code pairs for genderizing
artist.firstNames <- artists %>%
  select(first_name, iso3166) %>%
  distinct() %>%
  arrange(iso3166, first_name) %>%
  # NAs won't work if we go to CSV and read them back in!
  mutate(iso3166 = ifelse(is.na(iso3166), "none", iso3166))
print(nrow(artist.firstNames))
write.csv(artist.firstNames, "names_to_genderize.csv", row.names = FALSE)
