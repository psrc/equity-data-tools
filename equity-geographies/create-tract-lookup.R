library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)

# Load API key from environment
Sys.getenv("CENSUS_API_KEY")

# Variables ---------------------------------------------------------------
pop.vars <- c("001","003","004","005","006","007","008","009","012","999")
psrc.county <- c("King County","Kitsap County","Pierce County","Snohomish County")

yr <- 2019
acs <- "acs5"
tract.population.threshold <- 25
pop.tbl <- "B03002"

# Spatial Layers ----------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326

tract.lyr <- st_read(gdb.nm, "dbo.tract2010_nowater", crs = spn)

variable.labels <- load_variables(yr, acs, cache = TRUE) %>% rename(variable = name)

# Population Data for Regional Threshold ----------------------------------------------------
  
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = pop.tbl) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")

region <- county.tbl %>%
  select(variable, estimate, moe) %>%
  group_by(variable) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(GEOID="53033035053061", NAME="Region",ACS_Year=yr, ACS_Type=acs, ACS_Geography="Region")

results <- left_join(region,variable.labels,by=c("variable")) %>%
  mutate(race = str_extract(label, "(?<=!!)[^!!]*$"), race = gsub(":", "", race)) %>%
  filter(variable %in% paste0(pop.tbl,"_",pop.vars)) %>%
  select(-label,-concept)

non.white <- results %>%
  filter(race !="White alone", race !="Total") %>%
  select(GEOID, NAME, ACS_Geography, estimate, moe) %>%
  group_by(GEOID,NAME, ACS_Geography) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, variable=paste0(pop.tbl,"_","999"), race="non-White")

results <- bind_rows(list(results, non.white))

region.non.white.tot <- results %>% filter(race=="non-White") %>% select(estimate) %>% pull()
region.population.tot <- results %>% filter(race=="Total") %>% select(estimate) %>% pull()
region.non.white.shr <- region.non.white.tot / region.population.tot
  
# Remove extra stuff from memory
rm(county.tbl, region, non.white, results) 

# Tract Data --------------------------------------------------------------

# Download Tract data
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = acs, table = pop.tbl) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="Tract") %>%
  filter(variable %in% paste0(pop.tbl,"_",pop.vars))

results <- left_join(tract.tbl,variable.labels,by=c("variable")) %>%
  mutate(race = str_extract(label, "(?<=!!)[^!!]*$"), race = gsub(":", "", race))

# Calculate non-white population
non.white <- results %>%
  filter(race !="White alone", race !="Total") %>%
  select(GEOID, NAME, ACS_Geography, estimate, moe) %>%
  group_by(GEOID,NAME, ACS_Geography) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, variable=paste0(pop.tbl,"_","999"), race="non-White")

results <- bind_rows(list(results, non.white))

# Figure out Total population to calculate shares
totals <- results %>%
  filter(race=="Total") %>%
  select(NAME,estimate) %>%
  rename(total=estimate)

results <- left_join(results, totals, by=c("NAME")) %>%
  mutate(share=estimate/total) %>%
  select(-total)

# Remove extra stuff from memory
rm(tract.tbl, totals, non.white, variable.labels)

# Create Tract Look-up for non-white percentages ---------------------------
non.white <- results %>%
  filter(race=="non-White") %>%
  select(GEOID, estimate, moe, ACS_Year, ACS_Type, race, share ) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  mutate(over_half = case_when(
    share < 0.50 ~ 0,
    share >= 0.50 ~ 1)) %>%
  mutate(over_region = case_when(
    share < region.non.white.shr ~ 0,
    share >= region.non.white.shr ~ 1))

non.white.lyr <- left_join(tract.lyr, non.white, by = c("geoid10"="GEOID")) %>%
  st_transform(wgs84)

# write out csv for future use
write.csv(non.white.lyr %>% st_drop_geometry(),paste0("share_nonwhite_population_tract_acs5_",yr,".csv"))

# Create Map --------------------------------------------------------------

m.pal <- colorFactor(
  palette = c("white", "#C388C2"),
  levels = c(0, 1))

labels <- paste0(prettyNum(round(non.white.lyr$share*100, 1), big.mark = ","), "% non-white") %>% lapply(htmltools::HTML)

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addEasyButton(easyButton(
    icon="fa-globe", title="Region",
    onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
  
  addLayersControl(baseGroups = c("Base Map"),
                   overlayGroups = c("Over 50%","More than Regional Average"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  
  addPolygons(data=non.white.lyr,
              fillOpacity = 0.5,
              fillColor = ~m.pal(over_half),
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
                direction = "auto"),
              group = "Over 50%") %>%

  addPolygons(data=non.white.lyr,
              fillOpacity = 0.5,
              fillColor = ~m.pal(over_region),
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
                direction = "auto"),
              group = "More than Regional Average") %>%
  
  setView(lng=-122.257, lat=47.615, zoom=8.5) %>%
  
  hideGroup(c("More than Regional Average"))

