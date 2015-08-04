# Another look at the MoMA data, showing the number of artists per country.
# Of all the plots, this probably contains the most inaccuracies. 
# A lot of this code was inspired by: 
# http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
# I also got help from:
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# and a bug fix from:
# http://stackoverflow.com/questions/13662448/what-does-the-following-error-mean-topologyexception-found-non-nonded-intersec

require("rgeos")
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("RSQLite")
require("dplyr")


# Figure out how many artists came from each country ----------------------

momaDB <- src_sqlite("../momaDB.sqlite")
artists <- tbl(momaDB, "artists")
artistCounts <- artists %>%
  # Ugh, bug somewhere wiped out Chinese ISO codes :(
  select(birth_nationality, iso3166) %>%
  collect() %>%
  mutate(iso3166 = ifelse(is.na(iso3166) & grepl("^Chin", birth_nationality, ignore.case = TRUE),
                          "CN",
                          iso3166)) %>%
  # FIXME! This breaks my heart. Namibia happens to share the ISO 3166-1 alpha-2 code with missing data. 
  filter(iso3166 != "NA") %>%
  count(iso3166) %>%
  collect()


# Read in the shapefiles and turn them into DFs ---------------------------

# Read in the WGS84 bounding box shapefile and make its DF
bbox <- readOGR("ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bboxMoll <- spTransform(bbox, CRS("+proj=moll"))  # reproject bounding box
bboxMollDF <- fortify(bboxMoll)

# Graticule
grat <- readOGR("ne_110m_graticules_all", layer="ne_110m_graticules_15") 
gratMoll <- spTransform(grat, CRS("+proj=moll"))  # reproject graticule
gratMollDF <- fortify(gratMoll)

# Read in country borders shapefile
countries <- readOGR("ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries")
countriesMoll <- spTransform(countries, CRS("+proj=moll"))
# Fixes a bug in the shapefile
countriesMoll <- gBuffer(countriesMoll, width=0, byid=TRUE)

# Create a DF for the countries and add the iso_A2 tags to it
countriesMoll@data$id <- rownames(countriesMoll@data)
countriesMollDF <- fortify(countriesMoll, region="id")
countriesMoll.data <- select(countriesMoll@data, id, iso_a2)
countriesMollDF <- left_join(countriesMollDF, countriesMoll.data, by="id")


# Combine the artist counts with spatial data -----------------------------

countriesMollDF <- left_join(countriesMollDF, artistCounts, 
                             by = c("iso_a2" = "iso3166")) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         n_1 = pmax(n, 1))


# Draw the map ------------------------------------------------------------

colorScale <- round(10^seq(0, log10(max(artistCounts$n)), length.out=5), -1)

p1 <- ggplot(bboxMollDF, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=gratMollDF, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey50", alpha=0.2) +
  geom_polygon(data=countriesMollDF, aes(long,lat, group=group, fill=n_1)) + 
  geom_path(data=countriesMollDF, aes(long,lat, group=group), color="white", size=0.3) +
  scale_fill_gradient2(low = "#e5f5f9", mid = "#99d8c9", high = "#2ca25f",
                      name = "Count", trans = "log",
                      breaks = colorScale, labels = colorScale) + 
  labs(title="Artists at MoMA per country") + 
  coord_equal() + 
  list(theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.background = element_blank(),
             plot.background = element_rect(fill="#e6e8ed"),
             panel.border = element_blank(),
             axis.line = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             plot.title = element_text(size=16)))
print(p1)
ggsave("artists_per_country.png")
