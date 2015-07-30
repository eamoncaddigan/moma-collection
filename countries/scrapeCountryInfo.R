# Here's some code to pull in the tables I want for mapping countries to 
# adjectivals and their ISO 3166-1 alpha-2 codes

# For now, the CSVs this writes still need a little manual touch-up. It's not
# much data tho.

require(rvest)
require(dplyr)

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


# Countries and ISO 3166-1 alpha-2 codes ----------------------------------

countriesToCodes <- getTableFromWeb("https://en.wikipedia.org/wiki/ISO_3166-2",
                                    "//*[@id=\"mw-content-text\"]/table[1]")

colnames(countriesToCodes) <- c("iso3166", "country_name", "subdivisions")
countriesToCodes <- countriesToCodes %>%
  # Get rid of the wikipedia cruft
  mutate_each(funs(gsub("\\[.*\\]", "", .))) %>% 
  # Rearrange the country name into its natural order
  mutate(natural_country_name = sub("([[:alpha:]]*), ([[:alpha:]].*)", 
                                    "\\2 \\1", 
                                    country_name))


# Do a full join on the tables so I can tidy up by hand -------------------

countriesCodesAdjectivals <- countriesToCodes %>%
  select(natural_country_name, iso3166) %>%
  full_join(countriesToAdjectivals, by="natural_country_name")
if (file.exists("countries_codes_adjectivals.csv")) {
  #write.csv(countriesCodesAdjectivals, "countries_codes_adjectivals.csv", row.names = FALSE)
}


# Map nationalities to codes ----------------------------------------------

countriesCodesAdjectivals <- read.csv("countries_codes_adjectivals.csv")

nationalitiesToCodes <- countriesCodesAdjectivals %>% 
  select(-adjectivals) %>% 
  gather("adjectival_number", "adjectival", adjectival_1:adjectival_4) %>% 
  filter(!is.na(adjectival)) %>% 
  select(natural_country_name, adjectival, iso3166) %>% 
  gather("type", "nationality", natural_country_name, adjectival) %>% 
  select(nationality, iso3166) %>% 
  distinct()
write.csv(nationalitiesToCodes, "nationalities_codes.csv", row.names = FALSE)
