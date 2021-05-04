# This script performs the Calculations for Home Ownership by Race

# Values used in charts and text ------------------------------------------
region.own.nonwhite <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Home Ownership" & ACS_Race=="non-White" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

region.own.white <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Home Ownership" & ACS_Race=="White, not Hispanic or Latino" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

region.own.black <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Home Ownership" & ACS_Race=="Black or African American" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

region.own.all <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Home Ownership" & ACS_Race=="All" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

region.own.white <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Home Ownership" & ACS_Race=="White, not Hispanic or Latino" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

psrc.msa.own.all.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="MSA" & ACS_Concept=="Home Ownership" & ACS_Race=="All" & ACS_Category=="Owner-occupied housing units")) %>% 
  filter(!grepl('PR Metro Area', NAME)) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share)) %>%
  filter(NAME == "Seattle-Tacoma-Bellevue, WA Metro Area") %>%
  pull(rank)

psrc.msa.own.black.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="MSA" & ACS_Concept=="Home Ownership" & ACS_Race=="Black or African American" & ACS_Category=="Owner-occupied housing units")) %>% 
  filter(!grepl('PR Metro Area', NAME)) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share)) %>%
  filter(NAME == "Seattle-Tacoma-Bellevue, WA Metro Area") %>%
  pull(rank)

king.own.all <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Home Ownership" & ACS_Race=="All" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

king.own.black <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Home Ownership" & ACS_Race=="Black or African American" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

snohomish.own.all <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="Snohomish County" & ACS_Concept=="Home Ownership" & ACS_Race=="All" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)

snohomish.own.black <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="Snohomish County" & ACS_Concept=="Home Ownership" & ACS_Race=="Black or African American" & ACS_Category=="Owner-occupied housing units")) %>% 
  pull(share)


# Charts used in slides --------------------------------------------------
region.own.chart <- create.bar.chart.by.race(geo="Region",con="Home Ownership",cat=c("Owner-occupied housing units"))

county.own.chart <- create.bar.chart.by.race(geo="County",con="Home Ownership",cat=c("Owner-occupied housing units"))

msa.own.nonwhite.chart <- create.msa.bar.chart(con="Home Ownership", race="non-White",cat=c("Owner-occupied housing units"))
msa.own.all.chart <- create.msa.bar.chart(con="Home Ownership", race="All",cat=c("Owner-occupied housing units"))
msa.own.black.chart <- create.msa.bar.chart(con="Home Ownership", race="Black or African American",cat=c("Owner-occupied housing units"))


# Maps used in Slides -----------------------------------------------------
region.own.nonwhite.map <- create.tract.map.by.category(con="Home Ownership", cat="Owner-occupied housing units", race="non-White")
region.own.black.map <- create.tract.map.by.category(con="Home Ownership", cat="Owner-occupied housing units", race="Black or African American")
region.own.all.map <- create.tract.map.by.category(con="Home Ownership", cat="Owner-occupied housing units", race="All")
