# This'll be the code that adds gender data to the damn names

require(jsonlite)
require(httr)
require(dplyr)

ssl.verifypeer <- TRUE

# XXX - Needs to ALWAYS return a data.frame with five columns (or NULL)
lookupNames <- function(nameVector, countryCode) {
  query <- paste("name[", seq_along(nameVector), "]=", nameVector, sep="", collapse="&")
  if (countryCode != "none") {
    query <- paste(query, "&country_id=", countryCode, sep="")
  }
  r <- httr::GET("https://api.genderize.io", query = query)
  if (httr::status_code(r) == 200) {
    responseDF <- fromJSON(httr::content(r, as="text"))
  } else {
    cat('\n', httr::http_status(r)$message)
    cat('\n', httr::content(r)$error)
    if (httr::status_code(r) == 429){
      cat('\nYou have used all available requests in this subscription plan.')
    }
    responseDF <- NULL
  }
  return(responseDF)
}

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
