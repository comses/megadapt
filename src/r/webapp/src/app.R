library("dplyr")
library("ggplot2")
library("leaflet")
library("megadaptr")
library("rgeos")
library("rgdal")
library("shiny")
library("shiny.i18n")


model_cache_env <- new.env()
source("bootstrap.R", model_cache_env)
megadapt <- model_cache_env$model_cache$megadapt
cache <- model_cache_env$model_cache$cache
budget <- model_cache_env$model_cache$budget


#LANGUAGE SETTINGS
translator <- Translator$new(translation_json_path = "translation.json")
translator$set_translation_language("en")

#SHAPE FILES FOR MUNICIPALITIES
municipalities <<- readOGR(dsn = "municipiosDFCenso3", layer = "municipiosDFCenso3")


ui <- fluidPage(
  responsive = FALSE,
  titlePanel(" MEGADAPT MODEL"),
  sidebarLayout(

    sidebarPanel(
      #width =6,

      textOutput("choose_params"),


      p(),

      p(),

      p(),

      uiOutput("factor_chooser"),


       p(),
       selectInput('selected_language',
                   "Language",
                   choices = list("English" = "en", "Espanol" = "es"))
      #             selected = input$selected_language),

      #plotOutput("plot", width=400)
    ),
    mainPanel(

      leafletOutput("map", height =900, width = 800)
      #uiOutput("map_maker")

    )
  )
)


####-SERVER-####

server <- function(input, output, session) {

  # Make a Plot based on annual values of selected factor
  output$plot=renderPlot({

    data.for.plot <- plotData()
    title.list <- municipalityList()
    title.list <- names(title.list)
    plot.title <- title.list[as.numeric(input$select_municipality)]
    label.values <- data.for.plot[,1]
    label.values <- as.integer((label.values  / 2428) * 100)
    label.values <- as.character(label.values)
    label.values <- paste(label.values, "%", sep="")


    ggplot(data.for.plot, aes(x=data.for.plot[,1],y=data.for.plot[,2], group = 1))  +
      geom_line()+
      geom_point()+
      labs(title= plot.title, x=i18n()$t("Budget Scenarios"), y = factorName()) +
      scale_x_continuous(breaks = data.for.plot[,1], labels = label.values)

  })

  # Reactive expression for the data subsetted to what the user selected
  plotData <- reactive({

    budgets.tested <<- cache[["index"]][["budget"]]

    #Iterate through each Budget test and get the average value for the parameter selected
    values <- NULL
    for (val in budgets.tested){
      subsetdf = load_scenario(cache, budget == val)
      cvgeo.split <- t(sapply(subsetdf$cvgeo, function(x) substring(x, first=c(3), last=c(5))))#Split Municipality Numbers from Census ID
      municipality <- as.numeric(cvgeo.split)
      subsetdf <- cbind(subsetdf, municipality) #ADD Municipality Numbers from Census ID
      subsetdf$cvgeo <- NULL # get rid of non-numeric data
      subsetdf = subsetdf %>%
        group_by(censusblock_id) %>%
        summarise_all(funs(mean))

      #subsetdf contains all of the averages, but if only selecting one municipality
      if (input$select_municipality > 1) {
        subsetdf <- subsetdf[subsetdf$municipality == as.numeric(input$select_municipality), ]
      }

      avg <- mean(subsetdf[[input$select_factor]], na.rm = TRUE)
      values <- c(values, avg)
      values
    }
    #adjust the budget value to show the fraction of the total census blocks

    plot.df <- data.frame(budgets.tested,values)
    plot.df

  })


  output$choose_params <- renderText({
    i18n()$t("Choose from the Parameters Below to See Simulation Results")
  })


  output$map_maker <- renderUI({
    tagList(
    leafletOutput("map", height =700, width = 700)
    )
  })

  output$factor_chooser<- renderUI({

    tagList(
      selectInput("select_factor", i18n()$t("Simulation Factor"), choices = factorList()
      ),
      selectInput("select_municipality", i18n()$t("Municipality to Display"), choices = municipalityList()
      ),
      selectInput("select_budget", i18n()$t("Budget Scenarios"), choices = budgetList()
      ),
      plotOutput("plot")
    )
  })

  municipalityList <- reactive({

    places = list(i18n()$t("All Municipalities"), "Azcapotzalco", "Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero", "Iztacalco",
                  "Iztapalapa","La Magdalena Contreras","Milpa Alta","Álvaro Obregón","Tláhuac","Tlalpan",
                  "Xochimilco","Benito Juárez","Cuauhtémoc","Miguel Hidalgo","Venustiano Carranza")
    listvalues = (1:17)
    names(listvalues) <- places
    as.list(listvalues)

  })

  budgetList <- reactive({

    budgets.tested <- cache[["index"]][["budget"]]
    pcts.tested <- as.integer((budgets.tested  / 2428) * 100) #2428 Is the total number of cenusus units, or AGEB_ID
    pcts.tested <- as.character(pcts.tested)
    pcts.tested <- paste(pcts.tested, "%", sep="")
    names(budgets.tested) <- pcts.tested
    as.list(budgets.tested)
  })

  i18n <- reactive({

    selected <- input$selected_language

    if (length(selected) > 0) { # && selected %in% SUPPORTED_LANGUAGES
      translator$set_translation_language(selected)
    } else {
      translator$set_translation_language("en")
    }
    translator
  })

  factorName <- reactive({

     if(length(input$select_factor) < 1){
       name.of.factor = i18n()$t("Vulnerability of residents to potable water scarcity")
     }
    else{
      if (identical(input$select_factor, "potable_water_vulnerability_index")){name.of.factor = i18n()$t("Vulnerability indicator of residents to potable water scarcity")}
      if (identical(input$select_factor, "non_potable_water_vulnerability_index")){name.of.factor = i18n()$t("Vulnerability indicator of residents to flooding")}
      if (identical(input$select_factor, "potable_water_system_intervention_count")){name.of.factor = i18n()$t("Number of Actions on Potable Water Infrastructure")}
      if (identical(input$select_factor, "non_potable_water_system_intervention_count")){name.of.factor = i18n()$t("Number of Actions on Sewer and Drainage Infrastructure")}
      if (identical(input$select_factor,"potable_water_infrastructure_age")){name.of.factor = i18n()$t("Potable water infrastructure age")}
      if (identical(input$select_factor, "non_potable_water_infrastructure_age")){name.of.factor = i18n()$t("Non Potable water infrastructure age")}
      if (identical(input$select_factor,"days_no_potable_water")){name.of.factor = i18n()$t("Days without potable water")}
    }
    name.of.factor

  })

  factorList <- reactive({

    factor.names = list(i18n()$t("Vulnerability indicator of residents to potable water scarcity"),i18n()$t("Vulnerability indicator of residents to flooding"),i18n()$t("Number of Actions on Potable Water Infrastructure"),
                        i18n()$t("Number of Actions on Sewer and Drainage Infrastructure"),i18n()$t("Potable water infrastructure age") ,i18n()$t("Non Potable water infrastructure age"),i18n()$t("Days without potable water"))
    choices = list("potable_water_vulnerability_index","non_potable_water_vulnerability_index", "potable_water_system_intervention_count","non_potable_water_system_intervention_count",
                   "potable_water_infrastructure_age","non_potable_water_infrastructure_age", "days_no_potable_water")

    names(choices) <- factor.names
    as.list(choices)

  })


  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({

    if(length(input$select_factor) < 1){
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
    #colorNumeric("YlOrRd", studyArea_CVG.4.display@data[[input$select_factor]] )
    if(length(input$select_factor) < 1){
      colorBin("YlOrRd", studyArea_CVG.4.display@data[["potable_water_vulnerability_index"]], bins = 7)
    }
    else{
      colorBin("YlOrRd", studyArea_CVG.4.display@data[[input$select_factor]] , bins = 7)
    }




  })

  municipalityShapeColor <- reactive({
    all.values <- municipalities@data[["mun_num"]]
    all.values <- as.numeric(all.values)
    test = 1:20
    if(length(input$select_factor) < 1){
    colorBin("YlOrRd", all.values, bins = 5)
    }
    else{
          if (input$select_municipality > 1) {
             colorBin("YlOrRd", as.numeric(input$select_municipality), bins = 2)
           }else{
          colorBin("YlOrRd", all.values, bins = 17)
          }
    }
    # mypalette = colorNumeric( palette="viridis", domain=municipalities@data$mun_num, na.color="transparent")
    # if (input$select_municipality > 1) {
    #   mypalette = colorNumeric( palette="viridis", domain=input$select_municipality, na.color="transparent")
    # }
    # mypalette


  })


  output$map <- renderLeaflet({

    subsetdf <- load_scenario(cache, budget == 1200)
    subsetdf$cvgeo <- NULL
    subsetdf = subsetdf %>%
      group_by(censusblock_id) %>%
      summarise_all(funs(mean))
    x <- megadapt[["study_area"]]
    y <- subsetdf
    x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing original row order for later sorting
    x.dat <- as(x, "data.frame")  # Create new data.frame object
    x.dat <-as.data.frame(x.dat[,c(1,ncol(x.dat))])
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
    if(length(input$select_factor) < 1){
      values.4.fill = studyArea_CVG.4.display@data[["potable_water_vulnerability_index"]]
    }
    else{
      values.4.fill =  studyArea_CVG.4.display@data[[input$select_factor]]
    }


    #leafletProxy("map", data = filteredData()) %>%
    leafletProxy("map") %>%
      clearShapes() %>%
       addPolygons(data = municipalities, color = "#444444", weight = 1, smoothFactor = 1.0,
                             opacity = 0.5, fillOpacity = 0.5, fillColor = municipalityShapeColor(), #~pal( m.colors),
                             highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = FALSE)) %>%
       addPolygons(data = filteredData(), color = "#444444", weight = 1, smoothFactor = 1.0,
                opacity = 1.0, fillOpacity = 1.0,
                #label = studyArea_CVG.4.display@data[["ageb_id"]],
                #popup = paste(studyArea_CVG.4.display@data[["ageb_id"]]),
                fillColor = ~pal(values.4.fill )
                #highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
    )


    # %>%
      #addMarkers(data = studyArea_CVG.4.display@data, popup = studyArea_CVG.4.display@data[["ageb_id"]]

     # )

  })


  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = studyArea_CVG.4.display)


    # Remove any existing legend, and create a new one.
    proxy %>% clearControls()
    pal <- colorpal()
    if(length(input$select_factor) < 1){
      values.4.legend = studyArea_CVG.4.display@data[["potable_water_vulnerability_index"]]
    }
    else{
      values.4.legend =  studyArea_CVG.4.display@data[[input$select_factor]]
    }

    proxy %>% addLegend(position = "bottomright", title = factorName(),
                        pal = pal, values = values.4.legend, labels = c("Low", "","","Medium","", "High")
    )

  })



}

shinyApp(ui = ui, server = server)
