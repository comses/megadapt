#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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

model_cache_env <- new.env()
source("bootstrap.R", model_cache_env)
megadapt <- model_cache_env$model_cache$megadapt
cache <- model_cache_env$model_cache$cache
budget <- model_cache_env$model_cache$budget


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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Index Viewer"),
  dashboardSidebar(width = 350, sidebarMenu(
    id = 'tabs',
    menuItem('Potable Water Vulnerability', tabName = 'potable_water_vulnerability_index'),
    menuItem('Non Potable Water Vulnerability', tabName = 'non_potable_water_vulnerability_index'),
    menuItem('Number of actions on potable water infrastructure', tabName = 'potable_water_system_intervention_count'),
    menuItem('Number of actions on sewer and drainage infrastructure', tabName = 'non_potable_water_system_intervention_count'),
    menuItem('Potable water infrastructure age', tabName = 'potable_water_infrastructure_age'),
    menuItem('Non potable water infrastructure age', tabName = 'non_potable_water_infrastructure_age'),
    menuItem('Days without potable water', tabName = 'days_no_potable_water')
  ),
  uiOutput("budgetUI")
  ),
  dashboardBody(
    tags$head(tags$script(src = 'leafletMonkeyPatch.js')),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    fluidRow(
      box(leafletOutput('map'), width = 11)
      )
    )
)

mapIndexUI <- function() {
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 19.3326,
              lng = -99.14,
              zoom = 11) %>%
      addPolygons(
        data = megadapt$study_area,
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
    if (identical(input$tabs,'potable_water_vulnerability_index')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 1))
      style <- data.frame(fillColor = pal(filteredData()$potable_water_vulnerability_index))
      plot.values <- filteredData()$potable_water_vulnerability_index
    }
    if (identical(input$tabs,'non_potable_water_vulnerability_index')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 1))
      style <- data.frame(fillColor = pal(filteredData()$non_potable_water_vulnerability_index))
      plot.values <- filteredData()$non_potable_water_vulnerability_index
    }
    if (identical(input$tabs,'potable_water_system_intervention_count')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 5))
      style <- data.frame(fillColor = pal(filteredData()$potable_water_system_intervention_count))
      plot.values <- filteredData()$potable_water_system_intervention_count
    }
    if (identical(input$tabs,'non_potable_water_system_intervention_count')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 5))
      style <- data.frame(fillColor = pal(filteredData()$non_potable_water_system_intervention_count))
      plot.values <- filteredData()$non_potable_water_system_intervention_count
    }
    if (identical(input$tabs,'potable_water_infrastructure_age')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 100))
      style <- data.frame(fillColor = pal(filteredData()$potable_water_infrastructure_age))
      plot.values <- filteredData()$potable_water_infrastructure_age
    }
    if (identical(input$tabs,'non_potable_water_infrastructure_age')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 100))
      style <- data.frame(fillColor = pal(filteredData()$non_potable_water_infrastructure_age))
      plot.values <- filteredData()$non_potable_water_infrastructure_age
    }
    if (identical(input$tabs,'days_no_potable_water')){
      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 10))
      style <- data.frame(fillColor = pal(filteredData()$days_no_potable_water))
      plot.values <- filteredData()$days_no_potable_water
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
        title = input$tabs
      )



  })

  filteredData <- reactive({

    if(length(input$select_budget) < 1){
      budgets.tested <- cache[["index"]][["budget"]]
      subsetdf <- load_scenario(cache, budget == budgets.tested [1])
    }
    else{
      subsetdf <- load_scenario(cache, budget == input$select_budget)
    }


    subsetdf$cvgeo <- NULL # get rid of non-numeric data

    subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(mean))

    #Join the attribute tables
    x <- megadapt[["study_area"]]
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
    #x2@data <- x.dat2.ord  # Assign to shapefile the new data.frame
    #studyArea_CVG.4.display <<- x2
   x.dat2.ord

  })


  output$budgetUI <- renderUI({

    selectInput("select_budget", "Budget scenarios", choices = budgetList(),
                width = 150
    )
  })

  budgetList <- reactive({

    budgets.tested <- cache[["index"]][["budget"]]
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
