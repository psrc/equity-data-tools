library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)

# Load API key from environment
Sys.getenv("CENSUS_API_KEY")

psrc.county <- c("King County","Kitsap County","Pierce County","Snohomish County")

# Spatial Layers ----------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326

tract.lyr <- st_read(gdb.nm, "dbo.tract2010_nowater", crs = spn)

# Download Census Table ----------------------------------------------------
yr <- 2019
acs <- "acs5"
tract.population.threshold <- 25

c.tbl <- "B02015"

# Load labels for all variables in the dataset
variable.labels <- load_variables(yr, acs, cache = TRUE) %>% rename(variable = name)

# Download the data for all counties
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = c.tbl) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")

# Download Tract data
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = acs, table = c.tbl) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="Tract")

# Combine into one tibble for possible upload to elmer and add variable labels
census.data <- bind_rows(list(county.tbl, tract.tbl))

census.data <- left_join(census.data,variable.labels,by=c("variable")) %>%
  mutate(race = str_extract(label, "(?<=!!)[^!!]*$"), race = gsub(":", "", race))

# Remove extra stuff from memory
rm(county.tbl, tract.tbl)

# Create Map --------------------------------------------------------------
geo <- "Tract"
r <- "Filipino"

tbl <- census.data %>%
  filter((ACS_Geography == geo & race == r)) %>%
  select(GEOID,race,estimate) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  mutate(across(c('GEOID'), as.character))

c.layer <- left_join(tract.lyr,tbl, by = c("geoid10"="GEOID")) %>%
  st_transform(wgs84)

rng <- range(c.layer$estimate)
max_bin <- max(abs(rng))
round_to <- 10^floor(log10(max_bin))
max_bin <- ceiling(max_bin/round_to)*round_to
breaks <- (sqrt(max_bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2
bins <- c(0, breaks)

pal <- colorBin("Blues", domain = c.layer$estimate, bins = bins)

labels <- paste0(prettyNum(round(c.layer$estimate, -1), big.mark = ","), " people") %>% lapply(htmltools::HTML)

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addEasyButton(easyButton(
    icon="fa-globe", title="Region",
    onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
  
  addPolygons(data=c.layer,
              fillOpacity = 0.5,
              fillColor = pal(c.layer$estimate),
              opacity = 0.5,
              weight = 0.5,
              color = "#BCBEC0",
              dashArray = "",
              highlight = highlightOptions(
                weight =5,
                color = "76787A",
                dashArray ="",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  
  addLegend(pal = pal,
            values = c.layer$estimate,
            position = "bottomright",
            title = "Population") %>%

  setView(lng=-122.257, lat=47.615, zoom=8.5)

