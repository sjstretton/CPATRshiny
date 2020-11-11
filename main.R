## Purpose: This script contains the 'main function'.

################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(readxl)
library(magrittr)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(CPATR)
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")

gAnalysisEndYear <- 2030L
gBaseYear <- 2018L
gNumberOfYears <- 15L
gEndYear <- gBaseYear+gNumberOfYears-1
SelectedCarbonTax = 100
CarbonTaxTrajectoryForm <- read_csv("R/2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd")  #Shape of Carbon Tax Trajectory is Currently Standardised
#CarbonTaxInTime <-  CarbonTaxTrajectoryForm %>% mutate(CarbonTaxRate=ProportionOfMaxCTaxRate*SelectedCarbonTax)
CTRange=seq(from=0, to=100, by=20)


gResultsList=CPATR::OverallCoremodel(CountryList=TCAFCountryList,DoPower=TRUE,
                      BaseYear=2018L,NumberOfYears=15L,
                      AnalysisEndYear=2030L, EndYear=2033,
                      RetirementProportion=0.04,
                      CarbonTaxTrajectoryForm=read_csv("R/2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd"),
                      CTRange=seq(from=0, to=100, by=20))

MainSegmentedDataTable <- gResultsList[[1]] %>% filter(Year %in% as.character(gBaseYear:gAnalysisEndYear))   #Note that MainSegmentedDataTable uses character years
SelectedPowerDataByGenType <- gResultsList[[2]] %>% filter(Year %in% as.integer(gBaseYear:gAnalysisEndYear)) #Note that SelectedPowerDataByGenType uses integer years

save(MainSegmentedDataTable,CarbonTaxTrajectoryForm,CTRange,SelectedPowerDataByGenType,file="data/MainDataFile.rda")

write_csv(MainSegmentedDataTable,file="R/4-output/MainDataFile.csv")
write_csv(SelectedPowerDataByGenType,file="R/4-output/SelectedPowerDataByGenType.csv")


#Get Coal Cement from CPAT Results.
MST1.new <- read_csv("R/5-comparison/2ResultsFromCPAT/MST1-new.csv",col_types = "iiccccccidddddddddddddd",na = c("", "NA","#DIV/0!"))



