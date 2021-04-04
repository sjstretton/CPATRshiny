#This baby converts a big ugly dataframe into a nice array! Great stuff.
library(cubelyr)
library(rray)  #https://github.com/r-lib/rray


#View(MainSegmentedDataTable)
ChinaOnlyTable = MainSegmentedDataTable %>%  select(CountryCode, SubsectorCode,FuelType,Scenario,Model,CTScenarioRate,Year,CTRate, Emissions) %>%
  filter(CountryCode=="CHN")

MainTblCube = as.tbl_cube(ChinaOnlyTable, dim_names = c("CountryCode", "SubsectorCode","FuelType","Scenario","Model","CTScenarioRate","Year"))
#https://dplyr.tidyverse.org/reference/as.tbl_cube.html

EmissionsTblCube= MainTblCube %>% select(Emissions)

EmissionsTable= as.table(EmissionsTblCube)

EmissionsRRay <- rray(EmissionsTable)
#Next step: set the dimensions too!

# Compute proportions along the 1st dimension
EmissionsRRay / rray_sum(EmissionsRRay, axes = 1)