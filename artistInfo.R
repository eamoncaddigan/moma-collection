# This code reads in MoMA's Artworks.csv and creates a SQLite DB with two tables:
# * artworks: information about the pieces in MoMA's collection
# * artists: information about the people who made them

library(dplyr)
library(purrr)
library(tidyr)
library(RSQLite)

# Read in the info --------------------------------------------------------

momaData <- read.csv("Artworks.csv", stringsAsFactors = FALSE)
# Convert the venerable CamelCase column names to lowely snake_case. 
colnames(momaData) <- colnames(momaData) %>% 
  # Sorry museum branding guidelines
  gsub("MoMA", "moma", .) %>% 
  gsub("(([a-z])([A-Z]))", "\\2_\\3", .) %>% 
  tolower()

# Create a linking table to handle the artist-object reationship ----------

artistsObjects <- momaData %>%
  select(object_id, artist_bio, artist) %>%
  # Split the artist and artist_bio columns into lists.
  mutate(artist_bios_list = strsplit(artist_bio, "(?<=\\))[[:blank:]]*(?=\\()", 
                                     perl = TRUE),
         num_bios = map_int(artist_bios_list, length),
         artist_list = strsplit(artist, ",[[:blank:]]*"),
         num_artists = map_int(artist_list, length)) %>%
  # XXX: This currently removes 5952 records.
  filter(num_artists > 0,
         num_bios == num_artists)

# Create duplicate rows for each object with multiple artists.
artistsObjects <- artistsObjects[rep(seq(nrow(artistsObjects)), 
                                     artistsObjects$num_artists), ]

# Select one artist/artist_bio entry per row, and then create a unique artist_id
# for each unique artist/artist_bio entry. This also gets rid of the parens 
# around the bio.
artistsObjects <- artistsObjects %>% 
  group_by(object_id) %>%
  mutate(artist_index = row_number(num_artists)) %>%
  ungroup() %>%
  mutate(artist = map2_chr(artist_list, artist_index, ~ .x[.y]),
         artist_bio = sub("^\\((.*)\\)$", "\\1",
                          map2_chr(artist_bios_list, artist_index, ~ .x[.y])),
         artist_id = min_rank(paste(artist, artist_bio)))


# Normalize the DB into artist/object/and linking tables ------------------

# Create the artists table
artists <- artistsObjects %>%
  select(artist_id, artist, artist_bio) %>%
  distinct()

# Drop all of the redundant info from the linking table.
artistsObjects <- artistsObjects %>%
  select(object_id, artist_id)

# Drop all of the redundant info from the objects table.
artworks <- momaData %>%
  select(-artist, -artist_bio)


# Extract birth nation and nationality from the artist bio ----------------

# Not 100% happy with my approach here, but it uses a lot of functional
# programming so there's that.

# This function takes a pattern and other options, and returns a function that
# will apply that pattern to a string and use those other options to extract
# relevant info.
makePatternMatcher <- function(pattern, 
                               nationalityIndex = NA, 
                               birthNationIndex = NA, 
                               birthYearIndex = NA, 
                               deathYearIndex = NA,
                               isEntityDefault = FALSE) {
  # THESE functions return a data.frame containing the extracted artist_bio
  # information
  patternMatcher <- function(artistBio) {
    m <- regexec(pattern, artistBio)
    matches <- regmatches(artistBio, m)
    
    isMatch     <- map_lgl(matches, ~length(.x) > 0)
    nationality <- map_chr(matches, ~ifelse(length(.x) < nationalityIndex+1, NA, 
                                            .x[nationalityIndex+1]))
    birthNation <- map_chr(matches, ~ifelse(length(.x) < birthNationIndex+1, NA, 
                                            .x[birthNationIndex+1]))
    birthYear   <- map_int(matches, ~ifelse(length(.x) < birthYearIndex+1, NA, 
                                            as.integer(.x[birthYearIndex+1])))
    deathYear   <- map_int(matches, ~ifelse(length(.x) < deathYearIndex+1, NA, 
                                            as.integer(.x[deathYearIndex+1])))
    isEntity    <- isEntityDefault
    
    return(data_frame(is_match = isMatch, 
                      nationality, 
                      birth_nation = birthNation, 
                      birth_year = birthYear, 
                      death_year = deathYear, 
                      is_entity = isEntity))
  }
  
  return(patternMatcher)
}

patternFuns <- list()
# Nationality, born Country YYYY
# Nationality, born Country (now Other) YYYY
patternFuns[[1]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), born ([[:alpha:][:punct:][:space:]]+) ([[:digit:]]{4})$",
                                       nationalityIndex = 1, birthNationIndex = 2, birthYearIndex = 3)
# Nationality, born Country. YYYY-YYYY
patternFuns[[2]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), born ([[:alpha:][:punct:][:space:]]+)([[:digit:]]{4})[[:punct:]]([[:digit:]]{4})$",
                                       nationalityIndex = 1, birthNationIndex = 2, birthYearIndex = 3, deathYearIndex = 4)
# Nationality, born YYYY
patternFuns[[3]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), b(?:\\.|orn) ([[:digit:]]{4})$",
                                       nationalityIndex = 1, birthYearIndex = 2)
# Nationality, YYYY-YYYY
patternFuns[[4]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*),[[:space:]]*([[:digit:]]{4})[[:punct:]]+([[:digit:]]{4})$",
                                       nationalityIndex = 1, birthYearIndex = 2, deathYearIndex = 3)
# Nationality, born Country
patternFuns[[5]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), born ([[:alpha:][:punct:][:space:]]+)$",
                                       nationalityIndex = 1, birthNationIndex = 2)
# Nationality
patternFuns[[6]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*)$",
                                       nationalityIndex = 1)
# Nationality, (established|est.|founded) YYYY
patternFuns[[7]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), (?:(?:established|est\\.)|founded) ([[:digit:]]{4})$",
                                       nationalityIndex = 1, birthYearIndex = 2, isEntityDefault = TRUE)
# est. YYYY
patternFuns[[8]] <- makePatternMatcher("^est(?:\\.|ablished) ([[:digit:]]{4})$",
                                       birthYearIndex = 1, isEntityDefault = TRUE)
# Nationality, died YYYY
patternFuns[[9]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), died ([[:digit:]]{4})$",
                                       nationalityIndex = 1, deathYearIndex = 2)
# born YYYY
patternFuns[[10]] <- makePatternMatcher("^born ([[:digit:]]{4})$",
                                       birthYearIndex = 1)
# YYYY-YYYY
patternFuns[[11]] <- makePatternMatcher("^([[:digit:]]{4})[[:punct:]]+([[:digit:]]{4})$",
                                        birthYearIndex = 1, deathYearIndex = 2)
# Nationality, YYYY-?
patternFuns[[12]] <- makePatternMatcher("^([[:alpha:]]+), ([[:digit:]]{4})[[:punct:]]*$",
                                        nationalityIndex = 1, birthYearIndex = 2)
# Nationality, est. YYYY-YYYY
patternFuns[[13]] <- makePatternMatcher("^([[:alpha:]]+[[:blank:][:alpha:][:punct:]]*), (?:(?:established|est\\.)|founded) ([[:digit:]]{4})[[:punct:]]([[:digit:]]{4})$",
                                         nationalityIndex = 1, birthYearIndex = 2, deathYearIndex = 3, isEntityDefault = TRUE)


# Prepare to add the new biographical columns
commonCols <- c("nationality", "birth_nation", 
                "birth_year", "death_year", "is_entity")
for (c in commonCols) {
  artists[[c]] <- NA
}

# Apply the pattern-matching functions to the artist_bios. This keeps track of
# which rows have previously matched so that the patternFuns aren't each run on
# all of the data.
matchFalses <- rep(FALSE, length(artists$artist_bio))
matchedAlready <- matchFalses
for (patternFun in patternFuns) {
  # Apply patternFun to extract the biographical information for bios that
  # haven't been matched already.
  bioData <- patternFun(artists$artist_bio[!matchedAlready])
  
  # Where are new matches?
  matchedHere <- matchFalses
  matchedHere[!matchedAlready] <- bioData$is_match
  
  # Fill in the data for the new matches.
  artists[matchedHere, commonCols] <- bioData[bioData$is_match, commonCols]
  
  # Update the list of matched rows.
  matchedAlready <- matchedAlready | matchedHere
}

# Clean up nationality and birth_nation
artists <- artists %>% 
  mutate(nationality = ifelse(grepl("unknown", nationality, ignore.case = TRUE),
                              NA, nationality),
         nationality = sub(".*\\(now ([[:alpha:]][[:alpha:][:punct:][:space:]]*)\\).*", 
                           "\\1", nationality),
         nationality = sub(" \\((?:then|former) [[:alpha:]][[:alpha:][:punct:][:space:]]*\\)",
                           "", nationality),
         nationality = sub(", n\\.[[:space:]]?d\\.$", "", nationality),
         nationality = ifelse(nationality == "USA", "American", nationality),
         nationality = ifelse(nationality == "UK", "British", nationality),
         nationality = sub(", active.*", "", nationality),
         birth_nation = ifelse(grepl("^ca?\\.?$", birth_nation), NA, birth_nation),
         birth_nation = sub(".*\\(now ([[:alpha:]][[:alpha:][:punct:][:space:]]*)\\).*", 
                            "\\1", birth_nation),
         birth_nation = sub("[^[:alpha:]]*$", "", birth_nation),
         birth_nation = ifelse(grepl("U\\.?S\\.?A\\.?", birth_nation), 
                               "United States", birth_nation),
         birth_nation = sub(" \\(.*", "", birth_nation),
         birth_nation = sub("^ ?in ", "", birth_nation),
         birth_nation = sub(", born.*", "", birth_nation))

# # Add country codes to each artist ----------------------------------------
# 
# nationalitiesToCodes <- read.csv("countries/nationalities_codes.csv", stringsAsFactors = FALSE)
# artists <- artists %>%
#   # Add the country code if there is one
#   left_join(nationalitiesToCodes, by = c("birth_nationality" = "nationality"))
# 
# # Find unique first name / country code pairs for genderizing
# artist.firstNames <- artists %>%
#   select(first_name, iso3166) %>%
#   distinct() %>%
#   arrange(iso3166, first_name) %>%
#   # NAs won't work if we go to CSV and read them back in!
#   mutate(iso3166 = ifelse(is.na(iso3166), "none", iso3166))
# 
# if (!file.exists("genderize/names_to_genderize.csv")) {
#   write.csv(artist.firstNames, "genderize/names_to_genderize.csv", 
#             row.names = FALSE)
# } else {
#   nameGenders <- read.csv("genderize/names_with_genders.csv", 
#                           stringsAsFactors = FALSE) %>%
#     select(name, gender, country_id)
#   
#   # Add genders to the artists
#   artists <- artists %>%
#     # First, pretend we have no country info and get the genders that way
#     mutate(no_iso3166 = "none") %>%
#     left_join(nameGenders, by = c("first_name" = "name", "no_iso3166" = "country_id")) %>%
#     rename(no_country_gender = gender) %>%
#     # Now get the genders using the country info
#     left_join(nameGenders, by = c("first_name" = "name", "iso3166" = "country_id")) %>%
#     # Fill in missing gender info using the no-country gender info
#     mutate(gender = ifelse(is.na(gender), no_country_gender, gender)) %>%
#     # Drop the dummy columns
#     select(-no_iso3166, -no_country_gender)
#   
#   # Alright. All this CSV stuff is getting out of hand. Time for a RDB. :/
#   momaDB <- dbConnect(RSQLite::SQLite(), "momaDB.sqlite")
#   dbWriteTable(momaDB, "artists", artists)
#   dbDisconnect(momaDB)
# }
