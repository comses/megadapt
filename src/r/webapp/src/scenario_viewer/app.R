#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. You must have a dataframe loaded that contains the average of the model output runs
# The language can be changed below by changing language choice to en or es
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("dplyr")
library("ggplot2")
library("leaflet")
library("megadaptr")
library("rgeos")
library("rgdal")
library("shiny")
library("shiny.i18n")
library("shinydashboard")


#INPUT FILES
megadapt_census_blocks <- readOGR(dsn = "censusblocks", layer = "megadapt_wgs84")
names(megadapt_census_blocks@data)[1] <-"censusblock_id"
megadapt_census_blocks <- megadapt_census_blocks

#LANGUAGE SETTINGS
translator <- Translator$new(translation_json_path = "translation.json")
language_choice <- "es"
translator$set_translation_language(language_choice)   ### en for english or es for spanish

#INPUT DATA

if (!fs::file_exists('ex2.db')) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), 'ex2.db')
  params_df <- megadaptr:::params_cartesian_create(megadaptr::params_create(budget=1:5*200))
  megadaptr:::params_table_create(conn, 'ex', params_df)
  megadaptr:::results_table_create(conn, 'ex', params_df)
} else {
  conn <- DBI::dbConnect(RSQLite::SQLite(), 'ex2.db')
}

params <- dplyr::tbl(conn, 'ex_params')
input_data <- dplyr::tbl(conn, 'ex_results')

megadapt_results <- input_data %>% dplyr::collect()
megadapt_results$vulnerability <- sample(100, size = nrow(megadapt_results), replace = TRUE)
megadapt_results$resilience <- sample(100, size = nrow(megadapt_results), replace = TRUE)
megadapt_results$sensitivity <- sample(100, size = nrow(megadapt_results), replace = TRUE)

if(!("budget" %in% colnames(megadapt_results)))
{
  megadapt_results$budget <- sample(5, size = nrow(megadapt_results), replace = TRUE)
}
if(!("climate_scenario" %in% colnames(megadapt_results)))
{
  megadapt_results$climate_scenario <- sample(5, size = nrow(megadapt_results), replace = TRUE)
}


#municipalities <- readOGR(dsn = "municipiosDFCenso3", layer = "municipiosDFCenso3")


setStyle <- function(map, group, layerId, style) {
  invokeMethod(
    map = map,
    data = NULL,
    method = 'setStyle',
    group,
    layerId,
    style
  )
}


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Index Viewer"),
  dashboardSidebar(width = 350, sidebarMenu(
    id = 'tabs',
    menuItem(translator$t("Vulnerability"), tabName = 'vulnerability'),#NEW
    menuItem(translator$t("Resilience"), tabName = 'resilience'), #NEW
    menuItem(translator$t("Sensitivity"), tabName = 'sensitivity'), #NEW
    menuItem(translator$t("Household Sewer Vulnerability"), tabName = 'household_sewer_vulnerability'),
    menuItem(translator$t("Household Potable Water Vulnerability"), tabName = 'household_potable_water_vulnerability'),
    menuItem(translator$t("Resident Income Per Capita"), tabName = 'resident_income_per_capita'),#,
    menuItem(translator$t("Resident Income And Vulnerability"), tabName = 'resident_income_and_vulnerability')
    #menuItem('Number of actions on sewer and drainage infrastructure', tabName = 'non_potable_water_system_intervention_count'),
    #menuItem('Potable water infrastructure age', tabName = 'potable_water_infrastructure_age'),
    #menuItem('Non potable water infrastructure age', tabName = 'non_potable_water_infrastructure_age'),
    #menuItem('Days without potable water', tabName = 'days_no_potable_water')
  ),
  uiOutput("budgetUI"),
  uiOutput("climateUI")
  ),
  dashboardBody(
    tags$head(tags$script(src = 'leafletMonkeyPatch.js')),
    tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
    fluidRow(
      column(width = 11,
        box(width = 910,height = 900, solidHeader = TRUE,leafletOutput('map', height =870, width = 900))#,

        )#,
     # column(width = 4,
      # box(width = NULL, status = "warning",
      #    uiOutput("municSelector")#,
          #uiOutput("plot_view")
      # )
     # )

      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 19.3326,
              lng = -99.14,
              zoom = 11) %>%
      addPolygons(
        data = megadapt_census_blocks,
        layerId = ~ censusblock_id,
        fillColor = '#F00',
        fillOpacity = 0.9,
        weight = 0.2,
        color = '#444444',
        group = 'censusblocks'
      )
  })

  output$tabName <- reactive(paste0('Name: ', input$tabs))


  observe({

    layerId <- filteredData()$censusblock_id

    if (identical(input$tabs,'vulnerability')){
      plot.values <- filteredData()$vulnerability
      pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
      style <- data.frame(fillColor = pal(filteredData()$vulnerability))
      plot.title <- translator$t("Vulnerability")
    }
    if (identical(input$tabs,'resilience')){
      plot.values <- filteredData()$resilience
      pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
      style <- data.frame(fillColor = pal(filteredData()$resilience))
      plot.title <- translator$t("Resilience")
    }
    if (identical(input$tabs,'sensitivity')){
      plot.values <- filteredData()$sensitivity
      pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
      style <- data.frame(fillColor = pal(filteredData()$sensitivity))
      plot.title <- translator$t("Sensitivity")
    }

    if (identical(input$tabs,'household_sewer_vulnerability')){
      plot.values <- filteredData()$household_sewer_vulnerability
      pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
      style <- data.frame(fillColor = pal(filteredData()$household_sewer_vulnerability))
      plot.title <- translator$t("Household Sewer Vulnerability")

     }
     if (identical(input$tabs,'resident_income_per_capita')){
       plot.values <- filteredData()$resident_income_per_capita
       pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
       style <- data.frame(fillColor = pal(filteredData()$resident_income_per_capita))
       plot.title <- translator$t("Resident Income Per Capita")
     }
    if (identical(input$tabs,'household_potable_water_vulnerability')){
      plot.values <- filteredData()$household_potable_water_vulnerability
      pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
      style <- data.frame(fillColor = pal(filteredData()$household_potable_water_vulnerability))
      plot.title <- translator$t("Household Potable Water Vulnerability")
    }
    if (identical(input$tabs,'resident_income_and_vulnerability')){
      plot.values <- vulnerabilityData()
      pal <- colorNumeric(palette = "YlOrRd", domain = c(min(plot.values), max(plot.values)))
      style <- data.frame(fillColor = pal(vulnerabilityData()))
      plot.title <- translator$t("Resident Income And Vulnerability")
    }

    leafletProxy('map') %>%
      setStyle(group = 'censusblocks',
               layerId = layerId,
               style = style) %>%
      clearControls() %>%
      addLegend(
        position = c('bottomleft'),
        pal = pal,
        values = plot.values,
        title = plot.title #input$tabs
      )

  })

  filteredData <- reactive({

    v.budget = input$select_budget
    v.climate = input$select_climate
    resultsdf <- megadapt_results[ which(megadapt_results$budget==v.budget) , ]
    resultsdf <- resultsdf[ which(resultsdf$climate_scenario ==v.climate) , ]

    subsetdf = resultsdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(mean))

    #Join the attribute tables
    x <- megadapt_census_blocks
    y <- subsetdf
    x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing original row order for later sorting
    x.dat <- as(x, "data.frame")  # Create new data.frame object
    x.dat <-as.data.frame(x.dat[,c(1,ncol(x.dat))])
    x.dat2 <- merge(x.dat, y, by.x = "censusblock_id", by.y = "censusblock_id")  # Merge
    x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  # Reorder back to original
    x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  # Make new set of polygons, dropping those which arent in merge
    x2.dat <- as(x2, "data.frame")  # Make update x2 into a data.frame
    row.names(x.dat2.ord) <- row.names(x2.dat)  # Reassign row.names from original data.frame
    x.dat2.ord$sort_id <- NULL
    x.dat2.ord[is.na(x.dat2.ord)] <- 0
    x.dat2.ord
  })

  vulnerabilityData <- reactive({
    fdata <- filteredData()
    # use potable water vulnerability
    # some NA, ranges from 0.05 - 0.3
    fdata$household_potable_water_vulnerability <- fdata$household_potable_water_vulnerability * 100
    fdata[is.na(fdata)] <- 1
    high.income <- max(fdata$resident_income_per_capita)
    #income.values <- ((100 - (fdata$resident_income_per_capita / high.income)) * fdata$household_potable_water_vulnerability) + fdata$household_potable_water_vulnerability
    income.values <- ((1 - (fdata$resident_income_per_capita / high.income)) * fdata$household_potable_water_vulnerability) + fdata$household_potable_water_vulnerability

  })

  output$budgetUI <- renderUI({

    selectInput("select_budget", translator$t("Budget Scenarios"), choices = budgetList(),
                width = 150
    )
  })

  output$climateUI <- renderUI({

    selectInput("select_climate", translator$t("Climate Scenarios"), choices = climateList(),
                width = 150
    )
  })

  climateList <- reactive({
    climate.tested <- unique(megadapt_results$climate_scenario)
    as.list(climate.tested)
  })

  budgetList <- reactive({
    budgets.tested <- unique(megadapt_results$budget)
    pcts.tested <- as.integer((budgets.tested  / 2428) * 100) #2428 Is the total number of cenusus units, or AGEB_ID
    pcts.tested <- as.character(pcts.tested)
    pcts.tested <- paste(pcts.tested, "%", sep="")
    names(budgets.tested) <- pcts.tested
    as.list(budgets.tested)
  })


}

# Run the application
shinyApp(
  ui = ui,
  server = server,
  options = list(port = "5601", host = "0.0.0.0")
)
