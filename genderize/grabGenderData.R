# This'll be the code that adds gender data to the damn names

require(jsonlite)
require(httr)
require(dplyr)

genderizeKey <- NA
inputCSV <- "names_to_genderize.csv"


# Helper function grabs gender info for a vector of names -----------------

# Returns NA instead of NULL when a missing list element is requested, otherwise
# returns the element itself.
getListElement <- function(listName, elementName) {
  listElement <- NA
  if (!is.null(listName[[elementName]])) {
    listElement <- listName[[elementName]]
  }
  return(listElement)
}

lookupNames <- function(nameVector, countryCode = NA, apiKey = NA) {
  # Construct the query
  query <- paste("name[", seq_along(nameVector), "]=", nameVector, 
                 sep="", 
                 collapse="&")
  if (!is.na(countryCode) & (countryCode != "none")) {
    query <- paste(query, "&country_id=", countryCode, sep="")
  }
  if (!is.na(apiKey)) {
    query <- paste(query, "&apikey=", apiKey, sep="")
  }
  
  # Run it!
  queryResult <- GET("https://api.genderize.io", query = query)
  if (status_code(queryResult) == 200) {
    responseDF <- fromJSON(content(queryResult, as="text"))
    # Make sure this is a data.frame with the correct columns. I bet fromJSON 
    # can do this for me but I don't know how. This code works whether fromJSON 
    # returned a list (the response to one name) or a data.frame (the response
    # to several).
    responseDF <- data.frame(name = getListElement(responseDF, "name"),
                             gender = getListElement(responseDF, "gender"),
                             country_id = getListElement(responseDF, "country_id"),
                             probability = getListElement(responseDF, "probability"),
                             count = getListElement(responseDF, "count"),
                             stringsAsFactors = FALSE)
    responseDF <- mutate(responseDF, 
                         country_id = ifelse(is.na(country_id), "none", country_id))
    
  } else {
    cat(paste("\n!!!! http returned status code:",
              status_code(queryResult),
              "!!!! message:",
              http_status(queryResult)$message,
              "!!!! error:",
              content(queryResult)$error,
              sep="\n"))
    if (status_code(queryResult) == 429){
      cat('\n!!!! number of available requests exhaused')
    }
    responseDF <- NULL
  }
  return(responseDF)
}


# Read in the name and existing gender info -------------------------------

# Load the genderize.io supported countries
genderizeCountries <- fromJSON("countries.json")[[2]]

# Read in the DF of artist data
artistData <- read.csv(inputCSV, stringsAsFactors = FALSE)
artistData <- artistData %>%
  mutate(iso3166 = ifelse(iso3166 %in% genderizeCountries, iso3166, "none")) %>%
  arrange(iso3166, first_name)

# Read in the gender data we have so we don't keep querying the same people
genderData <- read.csv("names_with_genders.csv", stringsAsFactors = FALSE)
genderData <- mutate(genderData, looked_up = TRUE)
artistData <- artistData %>%
  left_join(genderData, 
            by = c("iso3166" = "country_id", "first_name" = "name")) %>%
  mutate(looked_up = ifelse(is.na(looked_up), FALSE, looked_up))
# Take the new column off of genderData
genderData <- select(genderData, -looked_up)

# Create a list of queries and run them -----------------------------------

# Break the data frame of name/country combos into a list of query chunks
artistData <- filter(artistData, !looked_up)
queryChunks = list()
countriesConsidered <- unique(artistData$iso3166)
for (c in seq_along(countriesConsidered)) {
  countrysNames <- artistData$first_name[artistData$iso3166 == countriesConsidered[c]]
  # Can only query up to 10 names at a time
  while(length(countrysNames) > 10) {
    queryChunks[[length(queryChunks)+1]] <- list(countriesConsidered[c], countrysNames[1:10])
    countrysNames <- countrysNames[11:length(countrysNames)]
  }
  queryChunks[[length(queryChunks)+1]] <- list(countriesConsidered[c], countrysNames)
}

# Now query all the chunks and store a list of DFs of their results
responseList <- list(genderData) # Start with what we already have
for (i in seq_along(queryChunks)) {
    responseDF <- lookupNames(queryChunks[[i]][[2]], queryChunks[[i]][[1]],
                              genderizeKey)
  if (is.null(responseDF)) {
    break
  } else {
    responseList[[length(responseList)+1]] <- responseDF
  }
}

# Combine the list into a single DF and write to a file
namesWithGenders <- do.call(rbind, responseList)
write.csv(namesWithGenders, "names_with_genders.csv", row.names = FALSE)
