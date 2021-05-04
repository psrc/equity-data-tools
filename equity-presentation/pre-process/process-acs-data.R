# Inputs ------------------------------------------------------------------
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")

pop.vars <- c("001","003","004","005","006","007","008","009","012","999")

# Functions ---------------------------------------------------------------
download.equity.data.acs <- function(c.yr=yr, c.tbl, c.acs=acs, t.type) {
  
  results <- NULL
  
  if (t.type=="subject") {c.var<-paste0(c.acs,"/subject")} else {c.var<-paste0(c.acs)}
  
  # Load labels for all variables in the dataset
  variable.labels <- load_variables(c.yr, c.var, cache = TRUE) %>% rename(variable = name)
  
  # Download the data for all counties
  county.tbl <- get_acs(geography = "county", state="53", year=c.yr, survey = c.acs, table = c.tbl) %>%
    mutate(NAME = gsub(", Washington", "", NAME)) %>%
    filter(NAME %in% psrc.county) %>%
    mutate(ACS_Year=c.yr, ACS_Type=c.acs, ACS_Geography="County")
  
  # Download the data for all places
  place.tbl <- get_acs(geography = "place", state="53", year=c.yr, survey = c.acs, table = c.tbl) %>%
    filter(!grepl('CDP', NAME)) %>%
    mutate(NAME = gsub(" city, Washington", "", NAME)) %>%
    mutate(NAME = gsub(" town, Washington", "", NAME)) %>%
    filter(NAME %in% psrc.cities) %>%
    mutate(ACS_Year=c.yr, ACS_Type=c.acs, ACS_Geography="Place")
  
  # Download the data for all msa's
  msa.tbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=c.yr, survey = c.acs, table = c.tbl) %>%
    filter(!grepl('Micro Area', NAME))
  
  msa.tbl <- msa.tbl %>%
    filter(GEOID %in% msa.list) %>%
    mutate(ACS_Year=c.yr, ACS_Type=c.acs, ACS_Geography="MSA")
  
  # Download Tract data
  tract.tbl <- get_acs(geography = "tract", state="53", year=c.yr, survey = c.acs, table = c.tbl) %>%
    filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
    mutate(NAME = gsub(", Washington", "", NAME)) %>%
    mutate(ACS_Year=c.yr, ACS_Type=c.acs, ACS_Geography="Tract")
  
  # Get a region total and add it to the county and place table
  region <- county.tbl %>%
    select(variable, estimate, moe) %>%
    group_by(variable) %>%
    summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
    rename(estimate=sumest, moe=summoe) %>%
    mutate(GEOID="53033035053061", NAME="Region",ACS_Year=c.yr, ACS_Type=c.acs, ACS_Geography="Region")
  
  results <- bind_rows(list(county.tbl, region, place.tbl, msa.tbl, tract.tbl))
  results <- left_join(results,variable.labels,by=c("variable"))
  
  if (t.type=="subject") {
    results <- results %>% 
      filter(!grepl('Percent', label), grepl('RACE', label)) %>%
      separate(variable, c("ACS_Table", "ACS_Subject","ACS_Variable"), "_")
  }
  
  if (t.type=="detailed") {
    results <- results %>%
      separate(variable, c("ACS_Table", "ACS_Variable"), "_") %>%
      mutate(ACS_Subject="C01") %>%
      mutate(label = gsub("Estimate!!","",label)) %>%
      mutate(label = gsub(":","",label)) %>%
      separate(label, c("temp", "ACS_Hispanic_Origin","ACS_Race","ACS_Race_Category"), "!!") %>%
      select(-temp) %>%
      mutate(ACS_Hispanic_Origin = replace_na(ACS_Hispanic_Origin,"Total"), ACS_Race = replace_na(ACS_Race,"Total"), ACS_Race_Category = replace_na(ACS_Race_Category,"All")) %>%
      filter(ACS_Race_Category=="All") %>%
      select(-ACS_Race_Category,-concept) %>%
      mutate(ACS_Category="Population")
    
  }  
  
  return(results)
  
}

# Population by Race ------------------------------------------------------
msa.tbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=yr, survey = acs, table = population) %>%
  filter(!grepl('Micro Area', NAME))

# Get list of MSA's with a population above the MSA threshold to keep (as well as both Seattle Area MSA's)
msa.list <- msa.tbl %>% filter(variable=="B03002_001") %>% filter((estimate>=msa.pop.limit | GEOID %in% psrc.msa)) %>% pull(GEOID)

rm(msa.tbl)

pop.by.race <- download.equity.data.acs(c.tbl=population, t.type="detailed")

pop.by.race <- pop.by.race %>%
  filter(ACS_Variable %in% pop.vars) %>%
  mutate(ACS_Race = if_else(ACS_Variable=="012","Hispanic or Latino origin",ACS_Race)) %>%
  mutate(ACS_Concept="Population") %>%
  mutate(ACS_Category = ACS_Hispanic_Origin) %>%
  select(-ACS_Hispanic_Origin) %>%
  mutate(ACS_Race = gsub(" alone","",ACS_Race)) %>%
  mutate(ACS_Race = gsub("White","White, not Hispanic or Latino",ACS_Race)) %>%
  mutate(ACS_Category = "Population")

non.white <- pop.by.race %>%
  filter(ACS_Race !="White, not Hispanic or Latino", ACS_Race !="Total") %>%
  select(GEOID, NAME, ACS_Geography, estimate, moe) %>%
  group_by(GEOID,NAME, ACS_Geography) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Table=population, ACS_Variable="999", ACS_Race="non-White", ACS_Category="Population", ACS_Concept="Population", ACS_Subject="C01")

pop.by.race <- bind_rows(list(pop.by.race, non.white))

totals <- pop.by.race %>%
  filter(ACS_Variable=="001") %>%
  select(NAME,estimate) %>%
  rename(total=estimate)

pop.by.race <- left_join(pop.by.race, totals, by=c("NAME")) %>%
  mutate(share=estimate/total) %>%
  filter(ACS_Variable %in% pop.vars) %>%
  select(-total)

rm(non.white,totals)

# Home Ownership by Race --------------------------------------------------
own.by.race <- download.equity.data.acs(c.tbl=ownership, t.type="subject")

own.by.race <- own.by.race %>%
  mutate(label = gsub("Estimate!!","",label)) %>%
  mutate(label = gsub("One race --!!","",label)) %>%
  separate(label, c("ACS_Category", "t1","ACS_Hispanic_Origin","ACS_Race"), "!!") %>%
  select(-t1,-ACS_Hispanic_Origin,-concept) %>%
  filter(ACS_Race != "White") %>%
  mutate(ACS_Race = gsub(" alone","",ACS_Race)) %>%
  mutate(ACS_Concept="Home Ownership")

any.race <- own.by.race %>%
  select(GEOID,NAME,ACS_Geography,ACS_Subject,ACS_Category,estimate,moe) %>%
  group_by(GEOID,NAME,ACS_Geography,ACS_Subject,ACS_Category) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Table=ownership, ACS_Variable="001", ACS_Race="All", ACS_Concept="Home Ownership", ACS_Subject="C01") %>%
  mutate(ACS_Subject = case_when(
    ACS_Category == "Occupied housing units" ~ "C01",
    ACS_Category == "Owner-occupied housing units" ~ "C03",
    ACS_Category == "Renter-occupied housing units" ~ "C05"))
  
own.by.race <- bind_rows(list(own.by.race, any.race))

non.white <- own.by.race %>%
  filter(ACS_Race !="White, not Hispanic or Latino", ACS_Race !="All") %>%
  select(GEOID, NAME, ACS_Geography, ACS_Category, estimate, moe) %>%
  group_by(GEOID,NAME, ACS_Geography, ACS_Category) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Table=ownership, ACS_Variable="999", ACS_Race="non-White", ACS_Concept="Home Ownership", ACS_Subject="C01") %>%
  mutate(ACS_Subject = case_when(
    ACS_Category == "Occupied housing units" ~ "C01",
    ACS_Category == "Owner-occupied housing units" ~ "C03",
    ACS_Category == "Renter-occupied housing units" ~ "C05"))

own.by.race <- bind_rows(list(own.by.race, non.white))

totals <- own.by.race %>%
  filter(ACS_Subject=="C01") %>%
  select(NAME,ACS_Variable,estimate) %>%
  rename(total=estimate)

own.by.race <- left_join(own.by.race, totals, by=c("NAME","ACS_Variable")) %>%
  mutate(share=estimate/total) %>%
  select(-total)

rm(any.race,non.white,totals)

# Educational Attainment by Race ------------------------------------------
edu.by.race <- download.equity.data.acs(c.tbl=education, t.type="subject")

edu.by.race <- edu.by.race %>%
  filter(!grepl('Male', label), !grepl('Female', label)) %>%
  mutate(label = gsub("Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!","",label)) %>%
  separate(label, c("ACS_Race","ACS_Category"), "!!") %>%
  mutate(ACS_Category = replace_na(ACS_Category,"All Education Levels")) %>%
  mutate(ACS_Race = gsub(" alone","",ACS_Race), ACS_Race = gsub("Black","Black or African American",ACS_Race)) %>%
  mutate(ACS_Race = gsub("American Indian or Alaska Native","American Indian and Alaska Native",ACS_Race)) %>%
  mutate(ACS_Race = gsub("Hispanic or Latino Origin","Hispanic or Latino origin",ACS_Race)) %>%
  filter(ACS_Race != "White") %>%
  mutate(ACS_Concept="Educational Attainment") %>%
  select(-concept)

any.race <- edu.by.race %>%
  select(GEOID,NAME,ACS_Geography,ACS_Category,estimate,moe) %>%
  group_by(GEOID,NAME,ACS_Geography,ACS_Category) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Table=education, ACS_Race="All", ACS_Concept="Educational Attainment", ACS_Subject="C01") %>%
  mutate(ACS_Variable = case_when(
    ACS_Category == "All Education Levels" ~ "091",
    ACS_Category == "Bachelor's degree or higher" ~ "092",
    ACS_Category == "High school graduate or higher" ~ "093"))

edu.by.race <- bind_rows(list(edu.by.race, any.race))

non.white <- edu.by.race %>%
  filter(ACS_Race !="White, not Hispanic or Latino", ACS_Race !="All") %>%
  select(GEOID, NAME, ACS_Geography, ACS_Category, estimate, moe) %>%
  group_by(GEOID,NAME, ACS_Geography, ACS_Category) %>%
  summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
  rename(estimate=sumest, moe=summoe) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Table=ownership, ACS_Variable="999", ACS_Race="non-White", ACS_Concept="Educational Attainment", ACS_Subject="C01")

edu.by.race <- bind_rows(list(edu.by.race, non.white))

totals <- edu.by.race %>%
  filter(ACS_Category=="All Education Levels") %>%
  select(NAME,ACS_Race,estimate) %>%
  rename(total=estimate)

edu.by.race <- left_join(edu.by.race, totals, by=c("NAME","ACS_Race")) %>%
  mutate(share=estimate/total) %>%
  select(-total)

rm(any.race,non.white,totals)


# Median Income by Race ---------------------------------------------------
inc.by.race <- download.equity.data.acs(c.tbl=income, t.type="subject")

inc.by.race <- inc.by.race %>%
  filter(grepl('Median income', label)) %>%
  mutate(label = gsub("Estimate!!","",label), label = gsub("!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households","",label)) %>%
  mutate(label = gsub("!!One race--","",label), label = gsub(" alone","",label), label = gsub(" \\(of any race\\)","",label)) %>%
  separate(label, c("ACS_Category","ACS_Race"), "!!") %>%
  mutate(ACS_Concept="Median Income") %>%
  select(-concept) %>%
  mutate(ACS_Race = replace_na(ACS_Race,"All")) %>%
  filter(ACS_Race != "White", NAME != "Region")

totals <- inc.by.race %>%
  filter(ACS_Race=="All") %>%
  select(NAME,estimate) %>%
  rename(total=estimate)

inc.by.race <- left_join(inc.by.race, totals, by=c("NAME")) %>%
  mutate(share=estimate/total) %>%
  select(-total)

# Remove extra tables from memory
rm(non.white,totals)

equity.data.by.race <- bind_rows(list(pop.by.race, edu.by.race, own.by.race, inc.by.race))
fwrite(equity.data.by.race,"data/equity_data_by_race.csv")

rm(pop.by.race, edu.by.race, own.by.race, inc.by.race)
