# A request! https://twitter.com/mkurlandsky/status/626363245675286528

require(dplyr)
require(tidyr)
require(ggplot2)
require(RSQLite)

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
         !grepl(",", Artist)) %>%
  # Not sure why dups are making it through. Unicode?
  distinct()


# Extract birth nation and nationality from the artist bio ----------------

artists <- artists %>%   
  mutate(birth_nation = sub("U\\.S\\.A\\.", "United States", ArtistBio),
         birth_nation = ifelse(grepl("born [[:alpha:]]+", birth_nation),
                               sub(".*born ([[:alpha:][:space:]]*).*", "\\1", birth_nation),
                               NA),
         birth_nation = ifelse(birth_nation %in% c("c", "ca"), NA, birth_nation),
         birth_nation = sub("\\W*$", "", birth_nation),
         birth_nation = ifelse(birth_nation %in% c("US", "USA"), "United States", birth_nation),
         nationality = sub("^[^[:alpha:]]([[:alpha:][:space:]]*).*", "\\1", ArtistBio),
         nationality = sub("\\W*$", "", nationality),
         nationality = sub(" and .*", "", nationality),
         birth_nationality = ifelse(!is.na(birth_nation), birth_nation, nationality),
         first_name = sub("([[:alpha:]]*).*", "\\1", Artist))


# Add country codes to each artist ----------------------------------------

nationalitiesToCodes <- read.csv("countries/nationalities_codes.csv", stringsAsFactors = FALSE)
artists <- artists %>%
  # Add the country code if there is one
  left_join(nationalitiesToCodes, by = c("birth_nationality" = "nationality"))

# Find unique first name / country code pairs for genderizing
artist.firstNames <- artists %>%
  select(first_name, iso3166) %>%
  distinct() %>%
  arrange(iso3166, first_name) %>%
  # NAs won't work if we go to CSV and read them back in!
  mutate(iso3166 = ifelse(is.na(iso3166), "none", iso3166))

if (!file.exists("genderize/names_to_genderize.csv")) {
  write.csv(artist.firstNames, "genderize/names_to_genderize.csv", 
            row.names = FALSE)
} else {
  nameGenders <- read.csv("genderize/names_with_genders.csv", 
                          stringsAsFactors = FALSE) %>%
    select(name, gender, country_id)
  
  # Add genders to the artists
  artists <- artists %>%
    # First, pretend we have no country info and get the genders that way
    mutate(no_iso3166 = "none") %>%
    left_join(nameGenders, by = c("first_name" = "name", "no_iso3166" = "country_id")) %>%
    rename(no_country_gender = gender) %>%
    # Now get the genders using the country info
    left_join(nameGenders, by = c("first_name" = "name", "iso3166" = "country_id")) %>%
    # Fill in missing gender info using the no-country gender info
    mutate(gender = ifelse(is.na(gender), no_country_gender, gender)) %>%
    # Drop the dummy columns
    select(-no_iso3166, -no_country_gender)
  
  # Alright. All this CSV stuff is getting out of hand. Time for a RDB. :/
  momaDB <- dbConnect(RSQLite::SQLite(), "momaDB.sqlite")
  dbWriteTable(momaDB, "artists", artists)
  dbDisconnect(momaDB)
  unlink("momaDB.sqlite")
}
