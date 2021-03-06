library(tidyverse)
library(readxl)
library(magrittr)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(CPATR)

source("R/5-comparison/3-PostProcessing.R")
source("R/5-comparison/4-ModelComparison.R")
source("R/5-comparison/5-MakeGraphs.R")
ChosenCountry = "CHN"


ui <- fluidPage(
  titlePanel("Simple CPAT-R"),
  mainPanel(
    tabsetPanel(
      tabPanel("Big Developing Countries",
               plotOutput("country2030curves", width = "1000px", height = "700px")),
      tabPanel("Pick Specific Country",
               selectInput("ChosenCountry", "Select Country", DropdownCountryList),
               selectInput("SelectedCarbonTax", "Select Carbon Tax", CTRange),
               tableOutput("static")
               ),
      tabPanel("Sectoral comparison", plotOutput("sectoralsplit", width = "1000px", height = "700px")),
      tabPanel("Intermodel comparison", plotOutput("modelcomparison", width = "1000px", height = "700px"))
    )
  )
)


server <- function(input, output, session) {

  output$static <- renderTable(filter(SummaryResults.ByCountry.2030,CountryCode == input$ChosenCountry &
                                                CTScenarioRate == input$SelectedCarbonTax) %>%
                                 select(CountryName, BaselineEmissions, Emissions, EmissionsReduction))

  output$country2030curves <- renderPlot({
    country2030curves
  }, res = 96)

  output$sectoralsplit <- renderPlot({
    sectoralemissionsfaceted
    }, res = 96)

  output$modelcomparison <- renderPlot({
    InterModelComparison
  }, res = 96)

}

shinyApp(ui, server)
