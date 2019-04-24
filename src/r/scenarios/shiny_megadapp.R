library("dplyr")
library("ggplot2")
library("leaflet")
library("megadaptr")
library("rgeos")
library("rgdal")
library("shiny")

source('util.R')

#define budget
Budg=(2:24)*100
#First checks to see if a folder with data exists
cache_path <- "budget_experiment"
megadapt <- build_megadapt_model(
  data_root_dir = data_root_dir,
  mental_model_file_names = mental_model_file_names
)
if (fs::dir_exists(cache_path)) {
  cache <- load_scenario_cache(cache_path)
} else {
  cache <- create_cartesian_scenario_cache(
    model = megadapt,
    path = cache_path,
    params = list(budget=Budg))
}

options(width = 1550)

ui <- fluidPage(
  responsive = FALSE,
  titlePanel(" MEGADAPT MODEL VIEWER"),
  sidebarLayout(

    sidebarPanel(
      #width =6,

      helpText("Choose from the parameters below to see simulation results"),

      p(),

      p(),

      p(),
      selectInput("select_factor", "Indicator",
                  choices = list("Vulnerability of residents to potable water scarcity" = "potable_water_vulnerability_index",
                                 "Vulnerability of residents to to flooding" = "non_potable_water_vulnerability_index",
                                 "Number of Actions on Potable Water Infrastructure" = "potable_water_system_intervention_count",
                                 "Number of Actions on Sewer and Drainage Infrastructure" = "non_potable_water_system_intervention_count",
                                 "Potable water infrastructure age" = "potable_water_infrastructure_age",
                                 "Non Potable water infrastructure age" = "non_potable_water_infrastructure_age",
                                 "Days without potable water" = "days_no_potable_water")
      ),
      selectInput("select_municipality", "Municipality to Display",
                  choices = list("All Municipalities" = 1, "Municipality 2" = 2, "Municipality 3" = 3, "Municipality 4" = 4, "Municipality 5" = 5, "Municipality 6" = 6,
                                 "Municipality 7" = 7,"Municipality 8" = 8,"Municipality 9" = 9,"Municipality 10" = 10,"Municipality 11" = 11,"Municipality 12" = 12,
                                 "Municipality 13" = 13,"Municipality 14" = 14,"Municipality 15" = 15,"Municipality 16" = 16,"Municipality 17" = 17)
      ),
      sliderInput("select_budget", "Budget:",
                  min = 200, max = 2400, value = 600, step = 200
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

    data.for.plot <- plotData()

    if (input$select_municipality > 1) {
      plot.title <- paste("Municipalty", input$select_municipality, sep = " ")
    }else{
      plot.title <- "All Municipalities"}

    ggplot(data.for.plot, aes(x=data.for.plot[,1],y=data.for.plot[,2]))  +
      geom_bar(stat="identity", width=15, fill="tomato3")  +
      labs(title= plot.title, x="Budget Scenarios", y = input$select_factor) +
      scale_x_continuous(breaks=data.for.plot[,1], labels=round(Budg/2400,1))

  })


  # Reactive expression for the data subsetted to what the user selected
  plotData <- reactive({

    cache <- load_scenario_cache(cache_path)
    budgets.tested <- cache[["index"]][["budget"]]
    values <<- NULL
    #Iterate through each Budget test and get the average value for the parameter selected
    for (val in budgets.tested){
      subsetdf = load_scenario(cache, budget == val)
      cvgeo.split <- t(sapply(subsetdf$cvgeo, function(x) substring(x, first=c(3), last=c(5))))#Split Municipality Numbers from Census ID
      municipality <- as.numeric(cvgeo.split)
      subsetdf <- cbind(subsetdf, municipality) #ADD Municipality Numbers from Census ID
      subsetdf$cvgeo <- NULL # get rid of non-numeric data
      subsetdf = subsetdf %>%
        group_by(censusblock_id) %>%
        summarise_all(funs(mean))

      if (input$select_municipality > 1) {
         subsetdf <- subsetdf[subsetdf$municipality == as.numeric(input$select_municipality), ]
       }

      avg <- mean(subsetdf[[input$select_factor]], na.rm = TRUE)
      values <<- c(values, avg)
    }
    #adjust the budget value to show the fraction of the total census blocks
    # budgets.tested = budgets.tested / 2400

    plot.df <- data.frame(budgets.tested,values)
    plot.df

  })


  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({

    subsetdf <- load_scenario(cache, budget == input$select_budget)
    subsetdf$cvgeo <- NULL # get rid of non-numeric data

    subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(mean))

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
    colorNumeric("YlOrRd", studyArea_CVG.4.display@data[[input$select_factor]] )

  })

  output$map <- renderLeaflet({

    subsetdf <- load_scenario(cache, budget == 600)
    subsetdf$cvgeo <- NULL
    subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(mean))
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
