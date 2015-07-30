# This'll be the code that adds gender data to the damn names

require(jsonlite)
require(httr)
require(dplyr)

ssl.verifypeer <- TRUE


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


# Load the genderize.io supported countries
genderizeCountries <- fromJSON("countries.json")[[2]]

genderData <- read.csv("names_to_genderize.csv", stringsAsFactors = FALSE)
genderData <- genderData %>%
  mutate(iso3166 = ifelse(iso3166 %in% genderizeCountries, iso3166, "none")) %>%
  arrange(iso3166, first_name)

# Break the data frame of name/country combos into a list of query chunks
queryChunks = list()
countriesConsidered <- c(genderizeCountries, "none")
for (c in seq_along(countriesConsidered)) {
  countryNames <- genderData$first_name[genderData$iso3166 == countriesConsidered[c]]
  # Can only query up to 10 names at a time
  while(length(countryNames) > 10) {
    queryChunks[[length(queryChunks)+1]] <- list(countriesConsidered[c], countryNames[1:10])
    countryNames <- countryNames[11:length(countryNames)]
  }
  queryChunks[[length(queryChunks)+1]] <- list(countriesConsidered[c], countryNames)
}

# Now query all the chunks
responseList <- list()
for (i in seq_along(queryChunks)) {
  responseDF <- lookupNames(queryChunks[[i]][[2]], queryChunks[[i]][[1]])
  if (is.null(responseDF)) {
    break
  } else {
    responseList[[length(responseList)+1]] <- responseDF
  }
}
namesWithGenders <- do.call(rbind, responseList)
write.csv(namesWithGenders, "names_with_genders.csv", row.names = FALSE)
