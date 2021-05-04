# This script performs the Calculations for Educational Attainment by Race

# Values used in charts and text ------------------------------------------
region.edu.nonwhite <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Educational Attainment" & ACS_Race=="non-White" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)

region.edu.black <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Educational Attainment" & ACS_Race=="Black or African American" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)

region.edu.all <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Educational Attainment" & ACS_Race=="All" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)

region.edu.white <- equity.data.by.race %>% 
  filter((ACS_Geography=="Region" & ACS_Concept=="Educational Attainment" & ACS_Race=="White, not Hispanic or Latino" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)

psrc.msa.edu.all.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="MSA" & ACS_Concept=="Educational Attainment" & ACS_Race=="All" & ACS_Category=="Bachelor's degree or higher")) %>% 
  filter(!grepl('PR Metro Area', NAME)) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share)) %>%
  filter(NAME == "Seattle-Tacoma-Bellevue, WA Metro Area") %>%
  pull(rank)

psrc.msa.edu.black.rank <- equity.data.by.race %>% 
  filter((ACS_Geography=="MSA" & ACS_Concept=="Educational Attainment" & ACS_Race=="Black or African American" & ACS_Category=="Bachelor's degree or higher")) %>% 
  filter(!grepl('PR Metro Area', NAME)) %>% 
  select(NAME,share) %>% 
  mutate(rank = rank(-share)) %>%
  filter(NAME == "Seattle-Tacoma-Bellevue, WA Metro Area") %>%
  pull(rank)

king.edu.all <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Educational Attainment" & ACS_Race=="All" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)

king.edu.black <- equity.data.by.race %>% 
  filter((ACS_Geography=="County" & NAME=="King County" & ACS_Concept=="Educational Attainment" & ACS_Race=="Black or African American" & ACS_Category=="Bachelor's degree or higher")) %>% 
  pull(share)


# Charts used in slides --------------------------------------------------
region.edu.attainment.chart <- create.bar.chart.by.race(geo="Region",con="Educational Attainment",cat=c("Bachelor's degree or higher"))

county.edu.attainment.bachelors.chart <- create.bar.chart.by.race(geo="County",con="Educational Attainment",cat=c("Bachelor's degree or higher"))
county.edu.attainment.highschool.chart <- create.bar.chart.by.race(geo="County",con="Educational Attainment",cat=c("High school graduate or higher"))

msa.edu.nonwhite.chart <- create.msa.bar.chart(con="Educational Attainment", race="non-White",cat=c("Bachelor's degree or higher"))
msa.edu.all.chart <- create.msa.bar.chart(con="Educational Attainment", race="All",cat=c("Bachelor's degree or higher"))
msa.edu.black.chart <- create.msa.bar.chart(con="Educational Attainment", race="Black or African American",cat=c("Bachelor's degree or higher"))


# Maps used in Slides -----------------------------------------------------
region.edu.nonwhite.map <- create.tract.map.by.category(con="Educational Attainment", cat="Bachelor's degree or higher", race="non-White")
region.edu.black.map <- create.tract.map.by.category(con="Educational Attainment", cat="Bachelor's degree or higher", race="Black or African American")
region.edu.all.map <- create.tract.map.by.category(con="Educational Attainment", cat="Bachelor's degree or higher", race="All")
