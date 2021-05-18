library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)

# Load API key 
#census_api_key('putkeyhere', install=TRUE)
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

c.tbl <- "B02001"
asian_data_code <- '_005'
asian.tbl<- paste(c.tbl,asian_data_code, sep='')

c.tbl.pi <- "B02001"
pi_data_code <- '_006'
pi.tbl<- paste(c.tbl,pi_data_code, sep='')

# Load labels for all variables in the dataset
variable.labels <- load_variables(yr, acs, cache = TRUE) %>% rename(variable = name)

# Download the data for all counties
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = c.tbl) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")

county.tbl <- county.tbl %>% filter((variable==asian.tbl) | (variable==pi.tbl))


# Download Tract data
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = acs, table = c.tbl) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="Tract")

tract.tbl <- tract.tbl %>% filter(variable==asian.tbl | variable==pi.tbl)

# Combine into one tibble for possible upload to elmer and add variable labels
census.data <- bind_rows(list(county.tbl, tract.tbl))

census.data <- left_join(census.data,variable.labels,by=c("variable")) %>%
  mutate(race = str_extract(label, "(?<=!!)[^!!]*$"), race = gsub(":", "", race))



# Create Map --------------------------------------------------------------
geo <- "Tract"
r1 <- "Asian alone"
r2<-'Native Hawaiian and Other Pacific Islander alone'
  
tbl <- census.data %>%
  filter(ACS_Geography == geo) %>%
  select(GEOID,race,estimate) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  mutate(across(c('GEOID'), as.character))%>%
  group_by(GEOID) %>%
  summarise(TotalPopulation=sum(estimate))

c.layer <- left_join(tract.lyr,tbl, by = c("geoid10"="GEOID")) %>%
  st_transform(wgs84)

rng <- range(c.layer$TotalPopulation)
max_bin <- max(abs(rng))
round_to <- 10^floor(log10(max_bin))
max_bin <- ceiling(max_bin/round_to)*round_to
breaks <- (sqrt(max_bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2
bins <- c(0, breaks)

pal <- colorBin("YlOrRd", domain = c.layer$TotalPopulation, bins = bins)

labels <- paste0("Census Tract ", c.layer$geoidstr, '<p></p>', 
                 'Asian and Pacific Islander population: ', prettyNum(round(c.layer$TotalPopulation, -1), big.mark = ",")) %>% lapply(htmltools::HTML)

m <- leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addProviderTiles("CartoDB.VoyagerOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Region",
    onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
  
  addPolygons(data=c.layer,
              fillOpacity = 0.7,
              fillColor = pal(c.layer$TotalPopulation),
              opacity = 0.7,
              weight = 0.7,
              color = "#BCBEC0",
              group="population",
              options = leafletOptions(pane = "polygons"),
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
            title = "Asian and Pacific Islander Population") %>%
  addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                   overlayGroups = c("map labels",
                                     "population"))%>%

  setView(lng=-122.257, lat=47.615, zoom=8.5)

m