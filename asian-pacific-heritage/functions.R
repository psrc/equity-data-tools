# Functions used for Data Analysis and Visualization Creations

stored.procedure.from.db <- function(srv.nm, db.nm, procedure.nm) {
  
  db.con <- dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = srv.nm,
                      database = db.nm,
                      trusted_connection = "yes")
  
  w.tbl <- DBI::dbGetQuery(db.con, procedure.nm)
  odbc::dbDisconnect(db.con)
  as_tibble(w.tbl)
  
  return(w.tbl)
}

get.county.census <- function(c.type, c.yr, c.table, c.geo="county:033,035,053,061", c.state="state:53", l.yr = label.yr) {
  
  # Download Table from API
  tbl.values <- suppressWarnings(getCensus(name = c.type, vintage = c.yr, vars = c("NAME",paste0("group(",c.table,")")),region = c.geo, regionin = c.state) %>%
    select(ends_with(c("E","M"))) %>%
    select(-state) %>%
    rename(Geography=NAME) %>%
    pivot_longer(cols=contains("_"), names_to="name", values_to="value") %>%
    mutate(Geography = str_replace(Geography, ", Washington", "")))
  
  # Get variable labels 
  tbl.vars <- listCensusMetadata(name = c.type, vintage = l.yr, type = "variables", group = c.table) %>%
    filter(grepl("(E|M)$", name)) %>%
    select(name,label) %>%
    mutate(label = gsub("!!"," ", label), label = gsub("Margin of Error","MoE", label), label = gsub(" Total:",":", label))
  
  # JOin values and labels
  tbl.values <- inner_join(tbl.values, tbl.vars, by="name") %>%
    select(-name) %>%
    pivot_wider(names_from = label)
  
  tbl.values[tbl.values == -555555555 ] <- 0
  
  # Add total for region with calculated MoE for county to region aggregation
  region.moe <- suppressWarnings(tbl.values %>% select(contains("MoE")) %>% mutate(PSRC=1) %>% group_by(PSRC) %>% summarise_all(moe_sum))
  region.tot <- tbl.values %>% select(!contains("MOE"),-Geography) %>% mutate(PSRC=1) %>% group_by(PSRC) %>% summarise_all(sum) 
  region <- inner_join(region.tot,region.moe,by="PSRC") %>% mutate(Geography="Central Puget Sound") %>% select(-PSRC)
  
  # Append Region Total to table
  tbl.values <- bind_rows(tbl.values,region)
  
  return(tbl.values)
  
}

return.value <-function(data=results, c.geo=c, c.year=c.yr, acs.typ, c.tbl, c.val ) {
  
  r <- data[[c.year]][['tables']][[acs.typ]][[c.tbl]] %>%
    filter(Geography %in% c.geo) %>%
    pull(c.val) %>%
    sum()
  
  return(r)
}

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