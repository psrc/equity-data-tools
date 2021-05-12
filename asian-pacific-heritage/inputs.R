# Variables and lists used for data analysis

database.name <- "Elmer"
server.name <- "AWS-PROD-SQL\\Sockeye"

yr <- list(2019)

new.tables <- 

census.tables <- list("B02001.1yr" = list("B02001","Race", "acs/acs1"),
                      "B05002.5yr" = list("B05002","Place of Birth by Nativity and Citizenship", "acs/acs5"),
                      "B08105A.5yr" = list("B08105A","Means of Transportation to Work White Alone", "acs/acs5"),
                      "B08105B.5yr" = list("B08105A","Means of Transportation to Work Black Alone", "acs/acs5"),
                      "B08105D.5yr" = list("B08105D","Means of Transportation to Work Asian Alone", "acs/acs5"),
                      "B01002A.5yr" = list("B01002A","Median Age White Alone", "acs/acs5"),
                      "B01002B.5yr" = list("B01002A","Median Age Black Alone", "acs/acs5"),
                      "B01002D.5yr" = list("B01002D","Median Age Asian Alone", "acs/acs5"),
                      "B06004D.5yr" = list("B06004D","Place of Birth in the US", "acs/acs5"),
                      "B07004D.5yr" = list("B07004D","Geographic Mobility in the Past Year", "acs/acs5"),
                      "B10051A.5yr" = list("B10051A","Grandparents living with children under 18 White", "acs/acs5"),
                      "B10051B.5yr" = list("B10051B","Grandparents living with children under 18 Black", "acs/acs5"),
                      "B10051D.5yr" = list("B10051D","Grandparents living with children under 18 Asian", "acs/acs5"),
                      "B12002A.5yr" = list("B12002A","Marital Status White", "acs/acs5"),
                      "B12002B.5yr" = list("B12002B","Marital Status Black", "acs/acs5"),
                      "B12002D.5yr" = list("B12002D","Marital Status  Asian", "acs/acs5"),
                      "B13002A.5yr" = list("B13002A","Women 15-50 Gave Birth in Past 12 months White", "acs/acs5"),
                      "B13002B.5yr" = list("B13002B","Women 15-50 Gave Birth in Past 12 months Black", "acs/acs5"),
                      "B13002D.5yr" = list("B13002D","Women 15-50 Gave Birth in Past 12 months Asian", "acs/acs5"),
                      "C16001.1yr" = list("C16001","Language Spoken at Home for the Population 5 Years and Over", "acs/acs1"),
                      "C16004.1yr" = list("C16004","Age by Language Spoken at Home by Ability to Speak English for the Population 5 Years and Over", "acs/acs1"),
                      "B19013A.1yr" = list("B19013A","Median Household Income in the Past 12 Months (White Alone Householder)", "acs/acs1"),
                      "B19013B.1yr" = list("B19013B","Median Household Income in the Past 12 Months (Black or African American Alone Householder)", "acs/acs1"),
                      "B19013C.1yr" = list("B19013C","Median Household Income in the Past 12 Months (American Indian and Alaskan Native Alone Householder)", "acs/acs1"),
                      "B19013D.1yr" = list("B19013D","Median Household Income in the Past 12 Months (Asian Alone Householder)", "acs/acs1"),
                      "B19013E.1yr" = list("B19013E","Median Household Income in the Past 12 Months (Native Hawaiian and Other Pacific Islander Alone Householder)", "acs/acs1"),
                      "B19013F.1yr" = list("B19013F","Median Household Income in the Past 12 Months (Some Other Race Householder)", "acs/acs1"),
                      "B19013G.1yr" = list("B19013G","Median Household Income in the Past 12 Months (Two or More Races Householder)", "acs/acs1"),
                      "B19013H.1yr" = list("B19013H","Median Household Income in the Past 12 Months (White Alone, Not Hispanic or Latino Householder)", "acs/acs1"),
                      "B19013I.1yr" = list("B19013I","Median Household Income in the Past 12 Months (Hispanic or Latino Householder)", "acs/acs1"),
                      "B02001.5yr" = list("B02001","Race", "acs/acs5"),
                      "B02015.5yr" = list("B02015","Asian Alone by Selected Groups", "acs/acs5"),
                      "B17001A.5yr" = list("B17001A","Poverty Status in the Past 12 Months by Sex by Age (White Alone)", "acs/acs5"),
                      "B17001B.5yr" = list("B17001B","Poverty Status in the Past 12 Months by Sex by Age (Black or African American Alone)", "acs/acs5"),
                      "B17001C.5yr" = list("B17001C","Poverty Status in the Past 12 Months by Sex by Age (American Indian and Alaskan Native Alone)", "acs/acs5"),
                      "B17001D.5yr" = list("B17001D","Poverty Status in the Past 12 Months by Sex by Age (Asian Alone)", "acs/acs5"),
                      "B17001E.5yr" = list("B17001E","Poverty Status in the Past 12 Months by Sex by Age (Native Hawaiian and Other Pacific Islander Alone)", "acs/acs5"),
                      "B17001F.5yr" = list("B17001F","Poverty Status in the Past 12 Months by Sex by Age (Some Other Race Alone)", "acs/acs5"),
                      "B17001G.5yr" = list("B17001G","Poverty Status in the Past 12 Months by Sex by Age (Two or More Races Alone)", "acs/acs5"),
                      "B17001H.5yr" = list("B17001H","Poverty Status in the Past 12 Months by Sex by Age (White Alone, Not Hispanic or Latino)", "acs/acs5"),
                      "B17001I.5yr" = list("B17001I","Poverty Status in the Past 12 Months by Sex by Age (Hispanic or Latino)", "acs/acs5")
                      
)

counties <- list("Central Puget Sound", "King County", "Kitsap County", "Pierce County", "Snohomish County")

race.categories <- c("Geography","Estimate Total",
                     "Estimate Total White alone", "MoE Total White alone",
                     "Estimate Total Black or African American alone", "MoE Total Black or African American alone",
                     "Estimate Total American Indian and Alaska Native alone" , "MoE Total American Indian and Alaska Native alone",
                     "Estimate Total Asian and Pacific Islander", "MoE Total Asian and Pacific Islander",
                     "Estimate Other Race or Two or More Races", "MoE Other Race or Two or More Races",
                     "Hispanic or Latino (of any race)",
                     "Estimate Total Minority", "MoE Total Minority")

poverty.ratio.categories <- c("Geography","Estimate Total",
                              "Estimate under 100%", "MoE under 100%",
                              "Estimate under 150%", "MoE under 150%",
                              "Estimate under 200%", "MoE under 200%")

poverty.race.tables <- list("B17001A" = "White",
                            "B17001B" = "Black or African American",
                            "B17001C" = "American Indian/Alaska Native",
                            "B17001D" = "Asian",
                            "B17001E" = "Native Hawaiian/Other Pacific Islander",
                            "B17001F" = "Some other race",
                            "B17001G" = "Two or more races",
                            "B17001H" = "White, not Hispanic or Latino",
                            "B17001I" = "Hispanic or Latino")

hhincome.race.tables <- list("B19013A" = "White",
                            "B19013B" = "Black or African American",
                            "B19013C" = "American Indian/Alaska Native",
                            "B19013D" = "Asian",
                            "B19013E" = "Native Hawaiian/Other Pacific Islander",
                            "B19013F" = "Some other race",
                            "B19013G" = "Two or more races",
                            "B19013H" = "White, not Hispanic or Latino",
                            "B19013I" = "Hispanic or Latino")




tbl1.colnames <- c("Estimate", "Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate","Estimate", "MoE")
tbl2.colnames <- c("Estimate", "Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl3.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl4.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl5.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Pct", "MoE")
tbl6.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Pct", "MoE")
tbl7.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Pct", "MoE")
