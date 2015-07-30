# This'll be the code that adds gender data to the damn names

require(jsonlite)
require(httr)
require(dplyr)

# Helper function grabs gender info for a vector of names -----------------

lookupNames <- function(nameVector, countryCode) {
  # Construct the query
  query <- paste("name[", seq_along(nameVector), "]=", nameVector, 
                 sep="", 
                 collapse="&")
  if (!is.na(foo) & (foo != "none")) {
    query <- paste(query, "&country_id=", countryCode, sep="")
  }
  
  # Run it!
  queryResult <- GET("https://api.genderize.io", query = query)
  if (status_code(queryResult) == 200) {
    responseDF <- fromJSON(content(queryResult, as="text"))
    # TODO, make sure this is a data.frame with the correct columns.
    
  } else {
    cat(paste("\n!!!! http returned status code:",
              status_code(queryResult),
              "!!!! message:",
              http_status(queryResult)$message,
              "!!!! error:",
              http_content(queryResult)$error,
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
artistData <- read.csv("names_to_genderize.csv", stringsAsFactors = FALSE)
artistData <- artistData %>%
  mutate(iso3166 = ifelse(iso3166 %in% genderizeCountries, iso3166, "none")) %>%
  arrange(iso3166, first_name)

# Read in the gender data we have so we don't keep querying the same people
genderData <- read.csv("names_with_genders.csv", stringsAsFactors = FALSE)
genderData <- genderData %>%
  mutate(looked_up = TRUE)
artistData <- artistData %>%
  left_join(genderData, 
            by = c("iso3166" = "country_id", "first_name" = "name")) %>%
  mutate(looked_up = ifelse(is.na(looked_up), FALSE, looked_up))

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

# # Now query all the chunks
# responseList <- list()
# for (i in seq_along(queryChunks)) {
#   responseDF <- lookupNames(queryChunks[[i]][[2]], queryChunks[[i]][[1]])
#   if (is.null(responseDF)) {
#     break
#   } else {
#     responseList[[length(responseList)+1]] <- responseDF
#   }
# }
# namesWithGenders <- do.call(rbind, responseList)
# write.csv(namesWithGenders, "names_with_genders.csv", row.names = FALSE)
