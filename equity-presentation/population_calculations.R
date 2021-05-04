# This script performs the Calculations for Total Population


# Values used in charts and text ------------------------------------------
region.non.white <- equity.data.by.race %>% filter((ACS_Geography=="Region" & ACS_Concept=="Population" & ACS_Race=="non-White")) %>% pull(share)
region.black <- equity.data.by.race %>% filter((ACS_Geography=="Region" & ACS_Concept=="Population" & ACS_Race=="Black or African American")) %>% pull(share)
region.white <- equity.data.by.race %>% filter((ACS_Geography=="Region" & ACS_Concept=="Population" & ACS_Race=="White, not Hispanic or Latino")) %>% pull(share)

king.non.white <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Population" & ACS_Race=="non-White")) %>% pull(share)
king.black <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Population" & ACS_Race=="Black or African American")) %>% pull(share)
king.white <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Population" & ACS_Race=="White, not Hispanic or Latino")) %>% pull(share)

kitsap.non.white <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="Kitsap County" & ACS_Concept=="Population" & ACS_Race=="non-White")) %>% pull(share)
kitsap.black <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="Kitsap County" & ACS_Concept=="Population" & ACS_Race=="Black or African American")) %>% pull(share)
kitsap.white <- equity.data.by.race %>% filter((ACS_Geography=="County" & NAME=="Kitsap County" & ACS_Concept=="Population" & ACS_Race=="White, not Hispanic or Latino")) %>% pull(share)

msa.nonwhite.count <- equity.data.by.race %>% filter((ACS_Geography=="MSA" & ACS_Race=="non-White" & ACS_Concept=="Population")) %>% filter(!grepl('PR Metro Area', NAME)) %>% pull(NAME) %>% length

psrc.msa.nonwhite.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="MSA" & ACS_Race=="non-White" & ACS_Concept=="Population")) %>% 
  filter(!grepl('PR Metro Area', NAME)) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share)) %>%
  filter(NAME == "Seattle-Tacoma-Bellevue, WA Metro Area") %>%
  pull(rank)

city.nonwhite.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="Place" & ACS_Race=="non-White" & ACS_Concept=="Population")) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share))

city.count <- city.nonwhite.rank %>% pull(rank) %>% length
city.most.diverse <- city.nonwhite.rank %>% filter(rank==1) %>% pull(NAME)
seattle.diverse <- city.nonwhite.rank %>% filter(NAME=="Seattle") %>% pull(rank)

# Charts used in slides ---------------------------------------------------
region.pop.by.race.chart <- create.bar.chart.by.race(geo="Region",con="Population",cat=c("Population"))
county.pop.by.race.chart <- create.bar.chart.by.race(geo="County",con="Population",cat=c("Population"))
msa.pop.nonwhite.chart <- create.msa.bar.chart(con="Population", race="non-White",cat=c("Population"))
msa.pop.black.chart <- create.msa.bar.chart(con="Population", race="Black or African American",cat=c("Population"))
city.nonwhite.chart <- create.city.bar.charts(race="non-White", con="Population", reg=region.non.white)
city.black.chart <- create.city.bar.charts(race="Black or African American", con="Population", reg=region.black)

# Creation of Layer for Tracts with more than the regional share --------

tbl <- equity.data.by.race %>%
  filter((ACS_Geography == "Tract" & ACS_Race == "non-White" & ACS_Concept == "Population")) %>%
  select(GEOID,share) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  mutate(across(c('GEOID'), as.character))

people.of.color.lyr <- left_join(tract.lyr, tbl, by = c("geoid10"="GEOID")) %>%
  select(geoid10, share) %>%
  mutate(people_of_color=0) %>%
  mutate(people_of_color = case_when(
    share >= region.non.white ~ "Greater than Region")) %>%
  mutate(people_of_color = replace_na(people_of_color,"Less than Region")) %>%
  filter(people_of_color=="Greater than Region") %>%
  st_union() %>% 
  st_sf() %>%
  mutate(people_of_color="Greater than Region") %>%
  st_transform(wgs84)

# Maps used in slides -----------------------------------------------------
region.non.white.map <- create.tract.map(con="Population", val="share", race="non-White")

rm(tbl)