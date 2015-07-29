# Still dealing with countries/languages/etc I didn't even LOOK for a package
# that did this for me. Dumb. 
# Here I'll try to merge everything into a single DF with country name,
# adjectival, and language code

library(dplyr)
library(tidyr)

# A one-to-many mapping of codes to language names is fine here -----------

languagesToCodes <- read.csv("languages_to_codes.csv",
                             stringsAsFactors = FALSE)
languagesToCodes <- languagesToCodes %>%
  select(-language_name) %>% 
  gather("language_number", "language_name", language_name_1:language_name_4) %>%
  filter(!is.na(language_name)) %>%
  select(-language_number)


# Now add the codes to the listing of languages for countries -------------

countriesToLanguages <- read.csv("countries_to_languages.csv",
                                 stringsAsFactors = FALSE)
countriesToLanguages <- left_join(countriesToLanguages, languagesToCodes, 
                                  by=c("first_language" = "language_name")) %>%
  # Thanks to obnoxious hand-editing, this only drops EIGHT countries
  filter(!is.na(iso639)) %>%
  select(country_name, iso639)


# Now add the codes/languages to the list of countries/adjectivals --------

countriesToAdjectivals <- read.csv("countries_to_adjectivals.csv", 
                                   stringsAsFactors = FALSE)

countriesToAdjectivals <- left_join(countriesToAdjectivals, countriesToLanguages, 
                                    by=c("natural_country_name" = "country_name")) %>% 
  # This only loses 75 countries. Not terrible
  filter(!is.na(iso639))


# These will be the DFs with which I interact -----------------------------

nationalityToCountry <- countriesToAdjectivals %>% 
  select(natural_country_name, adjectival_1:adjectival_4) %>% 
  gather("adjectival_number", "adjectival", adjectival_1:adjectival_4) %>% 
  filter(!is.na(adjectival)) %>% 
  select(nationality = adjectival, country = natural_country_name)

countryToLanguage <- countriesToAdjectivals %>%
  select(country = natural_country_name, iso639)
