# Here's a BUNCH of code to pull in the tables I want for mapping countries to
# adjectivals and languages and ISO-639-1 codes. Probably deserves its own
# package.

# For now, the CSVs this writes still need a little manual touch-up. It's not
# much data tho.

library(rvest)
library(dplyr)

getTableFromWeb <- function(url, xpath) {
  tableList <- url %>%
    html() %>%
    html_nodes(xpath=xpath) %>%
    html_table(fill=TRUE)
  return(tableList[[1]])
}


# Countries and their adjectival forms ------------------------------------

countriesToAdjectivals <- getTableFromWeb("https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations",
                                          "//*[@id=\"mw-content-text\"]/table[1]")

colnames(countriesToAdjectivals) <- sub(" ", "_", 
                                        tolower(colnames(countriesToAdjectivals)))

countriesToAdjectivals <- countriesToAdjectivals[2:nrow(countriesToAdjectivals),] %>% 
  select(country_name, adjectivals) %>% 
  # Get rid of the wikipedia cruft
  mutate_each(funs(gsub("\\[.*\\]", "", .))) %>% 
  # For later splitting of adjectivals
  mutate(adjectivals = sub(" or ", ", ", adjectivals)) %>%
  # Rearrange the country name into its natural order
  mutate(natural_country_name = sub("([[:alpha:]]*), ([[:alpha:]].*)", 
                                    "\\2 \\1", 
                                    country_name))
splitAdjectivals <- strsplit(countriesToAdjectivals[["adjectivals"]], ",[[:space:]]*")
for (i in seq_len(max(vapply(splitAdjectivals, length, 1)))) {
  countriesToAdjectivals[[paste("adjectival", i, sep="_")]] <- vapply(splitAdjectivals, function(x) { x[i] }, "")
}
write.csv(countriesToAdjectivals, "countries_to_adjectivals.csv", row.names = FALSE)


# Countries to languages --------------------------------------------------

countriesToLanguages <- getTableFromWeb("http://www.infoplease.com/ipa/A0855611.html",
                                        "//*[@id=\"Pg\"]/table[1]")

colnames(countriesToLanguages) <- c("country_name", "languages")
countriesToLanguages <- countriesToLanguages %>%
  mutate(first_language = sub("[[:space:]]*[[:punct:][:digit:]].*", "", languages),
         first_language = sub(" and .*", "", first_language))
write.csv(countriesToLanguages, "countries_to_languages.csv", row.names = FALSE)


# Languages to ISO-639-1 codes --------------------------------------------

languagesToCodes <- getTableFromWeb("https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes",
                                    "//*[@id=\"mw-content-text\"]/table[2]")

# Column names are difficult. Just hacking away here.
languagesToCodes <- languagesToCodes[, c(3, 5)]
colnames(languagesToCodes) <- c("language_name", "iso639")

# Some languages have multiple names
splitLanguageNames <- strsplit(languagesToCodes[["language_name"]], ",[[:space:]]*")
for (i in seq_len(max(vapply(splitLanguageNames, length, 1)))) {
  languagesToCodes[[paste("language_name_", i, sep="_")]] <- vapply(splitLanguageNames, function(x) { x[i] }, "")
}
write.csv(languagesToCodes, "languages_to_codes.csv", row.names = FALSE)

