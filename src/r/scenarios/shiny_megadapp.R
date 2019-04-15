library(shiny)
library(leaflet)
library(rgeos)
library(rgdal)
library(ggplot2)
library(dplyr)

#First checks to see if a folder with data exists
cache_path <- "budget_experiment"
if (fs::dir_exists(cache_path)) {
  cache <- load_scenario_cache(cache_path)
} else {
  cache <- build_scenario_cache(cache_path, list(budget=6:12*100))
}

ui <- fluidPage(
  titlePanel(" MEGADAPT MODEL VIEWER"),
  sidebarLayout(

    sidebarPanel(
      #width =5,

      helpText("Choose from the Parameters Below to See Simulation Results"),

      p(),

      p(),

      p(),
      selectInput("select_factor", "Simulation Factor",
                  choices = list("Vulnerability of residents to potable water scarcity" = "potable_water_vulnerability_index",
                                 "Vulnerability of residents to to flooding" = "non_potable_water_vulnerability_index",
                                 "Number of Actions on Potable Water Infrastructure" = "potable_water_system_intervention_count",
                                 "Number of Actions on Sewer and Drainage Infrastructure" = "non_potable_water_system_intervention_count",
                                 "Potable water infrastructure age" = "potable_water_infrastructure_age",
                                 "Non Potable water infrastructure age" = "non_potable_water_infrastructure_age",
                                 "Days without potable water" = "days_no_potable_water")
      ),
      selectInput("select_math", "Statistic to Display",
                  choices = list("Sum" = 1, "Minimum" = 2, "Maximum" = 3, "Mean" = 4)
      ),
      sliderInput("select_budget", "Budget:",
                  min = 600, max = 1200, value = 600, step = 100
                  #,animate = animationOptions(interval = 2000, loop = false)
      ),

      plotOutput("plot", width=400)
    ),
    mainPanel(

      leafletOutput("map", height =700, width = 700)

    )
  )
)


####-SERVER-####

server <- function(input, output, session) {

  # Make a Plot based on annual values of selected factor
  output$plot=renderPlot({

    budget = input$select_budget
    ii <- 2000:3000
    theme_set(theme_bw())
      ggplot(new_results, aes(x=year_sim,y=new_results[[input$select_factor]])) +
        geom_bar(stat="identity", width=.5, fill="tomato3") +
        labs(title= "Factor Over Time for all census blocks", x="Years") +
        scale_x_continuous(breaks=ii)



  })



  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({

    new_results <- load_scenario(cache, budget == input$select_budget)
    #New results is not modified since it is used by ggplot
    #Create a df with everything 'mathed'
    subsetdf <- new_results
    subsetdf$cvgeo <- NULL # get rid of non-numeric data
    if (input$select_math == 1) {
      subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(sum)) }
    if (input$select_math == 2) {
      subsetdf = subsetdf %>%
        group_by(censusblock_id) %>%
        summarise_all(funs(min)) }
    if (input$select_math == 3) {
      subsetdf = subsetdf %>%
        group_by(censusblock_id) %>%
        summarise_all(funs(max)) }
    if (input$select_math == 4) {
      subsetdf = subsetdf %>%
        group_by(censusblock_id) %>%
        summarise_all(funs(mean)) }

    #Join the attribute tables
    x <- megadapt[["study_area"]]
    y <- subsetdf
    x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing original row order for later sorting
    x.dat <- as(x, "data.frame")  # Create new data.frame object
    x.dat <-as.data.frame(x.dat[,c(1,49)])
    x.dat2 <- merge(x.dat, y, by.x = "ageb_id", by.y = "censusblock_id")  # Merge
    x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  # Reorder back to original
    x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  # Make new set of polygons, dropping those which arent in merge
    x2.dat <- as(x2, "data.frame")  # Make update x2 into a data.frame
    row.names(x.dat2.ord) <- row.names(x2.dat)  # Reassign row.names from original data.frame
    x.dat2.ord$sort_id <- NULL
    x2@data <- x.dat2.ord  # Assign to shapefile the new data.frame
    studyArea_CVG.4.display <<- x2


  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    #Use one parameter for the color
    colorNumeric("YlOrRd", studyArea_CVG.4.display@data[[input$select_math]] )
    colorNumeric("YlOrRd", studyArea_CVG.4.display@data[[input$select_factor]] )

  })

  output$map <- renderLeaflet({

    new_results <- load_scenario(cache, budget == 600)
    #New results is not modified since it is used by ggplot
    subsetdf <- new_results
    subsetdf$cvgeo <- NULL
    subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(sum))
    x <- megadapt[["study_area"]]
    y <- subsetdf
    x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing original row order for later sorting
    x.dat <- as(x, "data.frame")  # Create new data.frame object
    x.dat <-as.data.frame(x.dat[,c(1,49)])
    x.dat2 <- merge(x.dat, y, by.x = "ageb_id", by.y = "censusblock_id")  # Merge
    x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  # Reorder back to original
    x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  # Make new set of polygons, dropping those which arent in merge
    x2.dat <- as(x2, "data.frame")  # Make update x2 into a data.frame
    row.names(x.dat2.ord) <- row.names(x2.dat)  # Reassign row.names from original data.frame
    x.dat2.ord$sort_id <- NULL
    x2@data <- x.dat2.ord  # Assign to shapefile the new data.frame
    studyArea_CVG.4.display <<- x2



    cat("Running renderLeaflet")
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).

    leaflet() %>% addTiles() %>%
      setView(lat = 19.3326, lng =-99.14, zoom = 11)


  })

  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #math_type = input$select_math
     pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(studyArea_CVG.4.display@data[[input$select_factor]]  ),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      )
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = studyArea_CVG.4.display)

    # Remove any existing legend, and create a new one.
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomright", title = input$select_factor,
                       pal = pal, values = studyArea_CVG.4.display@data[[input$select_factor]]
    )

  })



}

shinyApp(ui = ui, server = server)
