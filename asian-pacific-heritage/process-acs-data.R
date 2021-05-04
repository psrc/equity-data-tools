# Inputs ------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")

acs <- "acs5"
yrs <- c(seq(2015,2019,1))

psrc.county <- c("53033","53035","53053","53061")
psrc.msa <- c("14740","42660")

pop.tbl <- "B03002"
pop.vars <- c("001","003","004","005","006","007","008","009","012")

inc.tbl <- "S1903"
inc.vars <- c("001","003","004","005","006","007","008","009","010")

edu.tbl <- "S1501"
edu.vars <- c("031","032","033","034","035","036","037","038","039",
              "040","041","042","043","044","045","046","047","048","049",
              "050","051","052","053","054")


ownership <- "S2502"


# Population ---------------------------------------------------------
acs.population <- NULL

census.tbl <- pop.tbl
keep.vars <- paste0(pop.tbl,"_",pop.vars)
total.var <- paste0(pop.tbl,"_001")

# Download the list of variables from the latest data year
variable.labels <- load_variables(max(yrs), acs, cache = TRUE) %>% rename(variable = name)

for (c.yr in yrs) {

  # Download County Level Data
  census.download <- get_acs(geography = "county", state="53", year=c.yr, survey = acs, table = census.tbl) %>%
    mutate(NAME = gsub(", Washington", "", NAME)) %>%
    filter(GEOID %in% psrc.county, variable %in% keep.vars)

  # Get a region total from the county data
  temp <- census.download %>%
    select(variable, estimate, moe) %>%
    group_by(variable) %>%
    summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
    rename(estimate=sumest, moe=summoe) %>%
    mutate(GEOID="53033035053061", NAME="Region", year=c.yr) 
  
  # Calculate Total population by geography
  totals <- temp %>% filter(variable==total.var) %>% select(NAME,estimate) %>% rename(total=estimate)
  
  # Add totals and calculate share of total by race
  temp <- left_join(temp, totals, by=c("NAME")) %>%
    mutate(share=estimate/total) %>%
    select(-total)  
  
  # Combine with other data years
  if (is.null(acs.population)) {acs.population <- temp} else {acs.population <- bind_rows(list(acs.population, temp))}
  
  rm(census.download,temp,totals)
  
}    
  
# Add labels from the latest census data year downloaded and clean up labels
acs.population <- left_join(acs.population,variable.labels,by=c("variable")) %>%
  mutate(concept="Population by Race", label = str_extract(label, "(?<=!!)[^!!]*$"), label = gsub(" alone", "", label), label = gsub(":", "", label)) %>%
  rename(race=label) %>%
  mutate(category="Population") %>%
  mutate(race=gsub("White","White, not Hispanic or Latino",race))


# Median Income ------------------------------------------------------------------
acs.income <- NULL

census.tbl <- inc.tbl
total.var <- paste0(inc.tbl,"_C03_001")

# Download the list of variables from the latest data year
variable.labels <- load_variables(max(yrs), paste0(acs,"/subject"), cache = TRUE) %>% rename(variable = name)

for (c.yr in yrs) {

  # Variables for income table changed in 2017 so make the keep lsit consistent depending on year
  if (c.yr <2017) {
    keep.vars <- paste0(inc.tbl,"_C02_",inc.vars)
  
  } else {
    keep.vars <- paste0(inc.tbl,"_C03_",inc.vars)
  }

  # Download Census Data by MSA for Median Income since we can't combine counties
  census.download <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=c.yr, survey = acs, table = census.tbl) %>%
    mutate(NAME = gsub(", WA Metro Area", " MSA", NAME)) %>%
    filter(GEOID %in% psrc.msa, variable %in% keep.vars)
  
  # Variable names changed in 2017 so adjust pre-2017 variable names to match so the labels align correctly
  if (c.yr <2017) {
    census.download <- census.download %>%
      mutate(variable = gsub("C02","C03",variable))
  }

  # Calculate Total population by geography
  totals <- census.download %>% filter(variable==total.var) %>% select(NAME,estimate) %>% rename(total=estimate)

  # Add totals and calculate share of total by race
  temp <- left_join(census.download, totals, by=c("NAME")) %>%
    mutate(share=estimate/total) %>%
    select(-total) 
  
  # Combine with other data years
  if (is.null(acs.income)) {acs.income <- temp} else {acs.income <- bind_rows(list(acs.income, temp))}
  
  rm(census.download,temp,totals)

}

# Add labels from the latest census data year downloaded and clean up labels
acs.income <- left_join(acs.income,variable.labels,by=c("variable")) %>%
  mutate(year=c.yr, concept="Median Income by Race") %>%
  mutate(race=label) %>%
  mutate(race = str_extract(race, "(?<=!!)[^!!]*$"), race = gsub("Households","Total",race), label="Median Income") %>%
  rename(category=label)

# Education ---------------------------------------------------------
acs.education <- NULL

census.tbl <- edu.tbl
keep.vars <- paste0(edu.tbl,"_C01_",edu.vars)

# Download the list of variables from the latest data year
variable.labels <- load_variables(max(yrs), paste0(acs,"/subject"), cache = TRUE) %>% rename(variable = name)

for (c.yr in yrs) {
  
  # Download County Level Data
  census.download <- get_acs(geography = "county", state="53", year=c.yr, survey = acs, table = census.tbl) %>%
    mutate(NAME = gsub(", Washington", "", NAME)) %>%
    filter(GEOID %in% psrc.county, variable %in% keep.vars)
  
  # Get a region total from the county data
  temp <- census.download %>%
    select(variable, estimate, moe) %>%
    group_by(variable) %>%
    summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate)) %>%
    rename(estimate=sumest, moe=summoe) %>%
    mutate(GEOID="53033035053061", NAME="Region", year=c.yr) 
  
  # Add Labels
  temp <- left_join(temp,variable.labels,by=c("variable"))
  
  # Calculate Total population by geography
  totals <- temp %>% filter(!grepl("Bachelor's", label),!grepl("High school", label)) %>% select(NAME,estimate) %>% rename(total=estimate)
  
  # Add totals and calculate share of total by race
  temp <- left_join(temp, totals, by=c("NAME")) %>%
    mutate(share=estimate/total) %>%
    select(-total)  
  
  # Combine with other data years
  if (is.null(acs.population)) {acs.population <- temp} else {acs.population <- bind_rows(list(acs.population, temp))}
  
  rm(census.download,temp,totals)
  
}    

# Add labels from the latest census data year downloaded and clean up labels
acs.population <- left_join(acs.population,variable.labels,by=c("variable")) %>%
  mutate(concept="Population by Race", label = str_extract(label, "(?<=!!)[^!!]*$"), label = gsub(" alone", "", label), label = gsub(":", "", label)) %>%
  rename(race=label) %>%
  mutate(category="Population") %>%
  mutate(race=gsub("White","White, not Hispanic or Latino",race))