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

megadapt <- megadapt_create(params_create())
study_area <- megadapt$study_area
column_choices <- tibble::tribble(
    ~label, ~value,
    'Resident Count', 'resident_count',
    'Resident Diarrhea/Pop', 'resident_diarrhea_per_capita',
    'Resident Asset Index', 'resident_asset_index')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Dynamic chloropleth"),
    dashboardSidebar(
        selectInput('column', label = 'Column', choices = column_choices %>% tidyr::spread(key = label, value = value), selected = column_choices[1])
    ),
    dashboardBody(
        tags$head(tags$script(src='leafletMonkeyPatch.js')),
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        leafletOutput('map')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lat = 19.3326, lng =-99.14, zoom = 11) %>%
            addPolygons(data = study_area, layerId = ~censusblock_id, fillColor = '#F00', weight = 0.2, color = '#444444', group = 'censusblocks')
    })

    fillColorPaletteCreator <- reactive({
        stopifnot(!is.null(input$column))
        colorBin("YlOrRd", study_area@data[,input$column], bins = 7)
    })

    observe({
        column <- study_area@data[,input$column]
        pal <- fillColorPaletteCreator()

        leafletProxy('map') %>%
            invokeMethod(data = NULL,
                         method = 'setStyle',
                         'censusblocks',
                         study_area@data$censusblock_id,
                         data.frame(fillColor = pal(column))) %>%
            clearControls() %>%
            addLegend(position = c('bottomleft'),
                      pal = pal, values = column, title = column_choices %>% tidyr::spread(key = value, value = label) %>% .[[input$column]])

        # js$setStyle(group = 'censusblocks', layerId = study_area@data$censusblock_id, style = data.frame(fillColor = pal(column)))
    })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = "5601", host = "0.0.0.0"))
