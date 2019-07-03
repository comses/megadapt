#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(megadaptr)
library(shiny)
library(shinydashboard)

megadapt <- megadapt_create(
  params_create(),
  mental_models = mental_model_sacmex_coupled_strategies(),
  flooding_fnss = flooding_index_fnss_create(),
  ponding_fnss = ponding_index_fnss_create()
)
megadapt <- megadaptr:::megadapt_initialize(megadapt)

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
  dashboardSidebar(sidebarMenu(
    id = 'tabs',
    menuItem('Ponding Index', tabName = 'ponding_index'),
    menuItem('Ponding Delta', tabName = 'ponding_delta'),
    menuItem('Flooding Index', tabName = 'flooding_index'),
    menuItem('Flooding Delta', tabName = 'flooding_delta')
  )),
  dashboardBody(
    tags$head(tags$script(src = 'leafletMonkeyPatch.js')),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    fluidRow(
      box(leafletOutput('map'), width = 9),
      tabItems(
        tabItem(tabName = 'ponding_index',
                fluidRow(
                  box(
                    h2("Ponding Index"),
                    textOutput('tabName'),
                    sliderInput(
                      "capacity",
                      "Non potable infrastructure capacity",
                      min = 0,
                      max = 20,
                      step = 1,
                      value = 5
                    ),
                    sliderInput(
                      "ponding",
                      "Average number of ponding reports in a year",
                      min = 0,
                      max = 20,
                      step = 1,
                      value = 5
                    ),
                    sliderInput(
                      "precipitation",
                      "Precipiation Volume",
                      min = 0,
                      max = 20,
                      step = 1,
                      value = 5
                    ),
                    sliderInput(
                      "runoff",
                      "Runoff Volume",
                      min = 0,
                      max = 20,
                      step = 1,
                      value = 5
                    ),
                    width = 3
                  )
                )),
        tabItem(tabName = 'ponding_delta', h2("Ponding Delta"),
        tabItem(tabName = 'ponding_delta',
                fluidRow(
                   box(
                     h2("Ponding Delta"),
                     sliderInput(
                       "capacityd",
                       "Non potable infrastructure capacity",
                       min = 0,
                       max = 20,
                       step = 1,
                       value = 5
                     ),
                     sliderInput(
                       "precipitationd",
                       "Precipiation Volume",
                       min = 0,
                       max = 20,
                       step = 1,
                       value = 5
                     ),
                     sliderInput(
                       "runoffd",
                       "Runoff Volume",
                       min = 0,
                       max = 20,
                       step = 1,
                       value = 5
                     ),
                     width = 3
                   )
                )
             ),
      tabItem(tabName = 'flooding_index', h2("Flooding Index"),
      tabItem(tabName = 'flooding_index',
              fluidRow(
                box(
                  h2("Flooding Index"),
                  sliderInput(
                    "floodingf",
                    "Flooding capacity",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "capacityf",
                    "Non potable infrastructure capacity",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "precipitationf",
                    "Precipiation Volume",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "runofff",
                    "Runoff Volume",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  width = 3
                )
              )
           ),
      tabItem(tabName = 'flooding_delta',
              fluidRow(
                box(
                  h2("Flooding Delta"),
                  sliderInput(
                    "floodingfd",
                    "Flooding capacity",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "capacityfd",
                    "Non potable infrastructure capacity",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "precipitationfd",
                    "Precipiation Volume",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  sliderInput(
                    "runofffd",
                    "Runoff Volume",
                    min = 0,
                    max = 20,
                    step = 1,
                    value = 5
                  ),
                  width = 3
                )
              )
      )

      )
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

    if (identical(input$tabs,'ponding_index')){
      weights <-
        c(
          capacity = input$capacity,
          ponding = input$ponding,
          precipitation = input$precipitation,
          runoff = input$runoff
        )
      ponding_index_fnss <- ponding_index_fnss_create(weights)
      ponding_index <-
        megadaptr:::call_fnss(ponding_index_fnss, study_data = megadapt$study_area@data)
      data <-
        megadaptr:::apply_data_changes(megadapt$study_area@data,
                                       ponding_index,
                                       megadaptr:::PK_JOIN_EXPR)

      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 1))

      layerId <- data$censusblock_id
      style <- data.frame(fillColor = pal(data$ponding_index))

      leafletProxy('map') %>%
        setStyle(group = 'censusblocks',
                 layerId = layerId,
                 style = style) %>%
        clearControls() %>%
        addLegend(
          position = c('bottomleft'),
          pal = pal,
          values = data$ponding_index,
          title = 'Ponding Index'
        )
    }
    if (identical(input$tabs,'ponding_delta')){
      weights <-
        c(
          capacity = input$capacityd,
          precipitation = input$precipitationd,
          runoff = input$runoffd
        )
      ponding_delta_fnss <- ponding_delta_method_fnss_create(weights)
      ponding_delta <-
        megadaptr:::call_fnss(ponding_delta_fnss, study_data = megadapt$study_area@data)
      data <-
        megadaptr:::apply_data_changes(megadapt$study_area@data,
                                       ponding_delta,
                                       megadaptr:::PK_JOIN_EXPR)

      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 25))

      layerId <- data$censusblock_id
      style <- data.frame(fillColor = pal(data$ponding_index))

      leafletProxy('map') %>%
        setStyle(group = 'censusblocks',
                 layerId = layerId,
                 style = style) %>%
        clearControls() %>%
        addLegend(
          position = c('bottomleft'),
          pal = pal,
          values = data$ponding_index,
          title = 'Ponding Delta'
        )
    }
    if (identical(input$tabs,'flooding_index')){
      weights <-
        c(
          flooding = input$floodingf,
          capacity = input$capacityf,
          precipitation = input$precipitationf,
          runoff = input$runofff
        )
      flooding_index_fnss <- flooding_index_fnss_create(weights)
      flooding_index <-
        megadaptr:::call_fnss(flooding_index_fnss, study_data = megadapt$study_area@data)
      data <-
        megadaptr:::apply_data_changes(megadapt$study_area@data,
                                       flooding_index,
                                       megadaptr:::PK_JOIN_EXPR)

      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0.5, 1))

      layerId <- data$censusblock_id
      style <- data.frame(fillColor = pal(data$flooding_index))

      leafletProxy('map') %>%
        setStyle(group = 'censusblocks',
                 layerId = layerId,
                 style = style) %>%
        clearControls() %>%
        addLegend(
          position = c('bottomleft'),
          pal = pal,
          values = data$flooding_index,
          title = 'Flooding Index'
        )
    }

    if (identical(input$tabs,'flooding_delta')){
      weights <-
        c(
          flooding = input$floodingfd,
          capacity = input$capacityfd,
          precipitation = input$precipitationfd,
          runoff = input$runofffd
        )

      flooding_delta_fnss <- flooding_delta_method_fnss_create(weights)
      flooding_delta <-
        megadaptr:::call_fnss(flooding_delta_fnss, study_data = megadapt$study_area@data)
      data <-
        megadaptr:::apply_data_changes(megadapt$study_area@data,
                                       flooding_delta,
                                       megadaptr:::PK_JOIN_EXPR)

      pal <- colorNumeric(palette = "YlOrRd",
                          domain = c(0, 8))

      layerId <- data$censusblock_id
      style <- data.frame(fillColor = pal(data$flooding_index))

      leafletProxy('map') %>%
        setStyle(group = 'censusblocks',
                 layerId = layerId,
                 style = style) %>%
        clearControls() %>%
        addLegend(
          position = c('bottomleft'),
          pal = pal,
          values = data$flooding_index,
          title = 'Flooding Delta'
        )
    }

  })


  # observe({
  #   weights <-
  #     c(
  #       capacity = input$capacity,
  #       ponding = input$ponding,
  #       precipitation = input$precipitation,
  #       runoff = input$runoff
  #     )
  #   ponding_index_fnss <- ponding_index_fnss_create(weights)
  #   ponding_index <-
  #     megadaptr:::call_fnss(ponding_index_fnss, study_data = megadapt$study_area@data)
  #   data <-
  #     megadaptr:::apply_data_changes(megadapt$study_area@data,
  #                                    ponding_index,
  #                                    megadaptr:::PK_JOIN_EXPR)
  #
  #   pal <- colorNumeric(palette = "YlOrRd",
  #                       domain = c(0, 1))
  #
  #   layerId <- data$censusblock_id
  #   style <- data.frame(fillColor = pal(data$ponding_index))
  #
  #   leafletProxy('map') %>%
  #     setStyle(group = 'censusblocks',
  #              layerId = layerId,
  #              style = style) %>%
  #     clearControls() %>%
  #     addLegend(
  #       position = c('bottomleft'),
  #       pal = pal,
  #       values = data$ponding_index,
  #       title = 'Ponding Index'
  #     )
  # })

}

# Run the application
shinyApp(
  ui = ui,
  server = server,
  options = list(port = "5601", host = "0.0.0.0")
)
