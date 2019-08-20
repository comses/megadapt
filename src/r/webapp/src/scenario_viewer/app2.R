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
megadapt_census_blocks <<- megadapt_census_blocks



#LANGUAGE SETTINGS
translator <<- Translator$new(translation_json_path = "translation.json")
language_choice <<- "es"
translator$set_translation_language(language_choice)   ### en for english or es for spanish



#INPUT DATA
#ponding_df <<- INPUT DATA
ponding_df <<- as.data.frame(lapply(ponding_df, as.numeric))
budgets.tested <- unique(ponding_df$budget)
climate.tested <- unique(ponding_df$climate_scenario)


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
  dashboardSidebar(width = 150,
  uiOutput("budgetUI"),
  uiOutput("climateUI")
  ),
  dashboardBody(
    #tags$head(tags$script(src = 'leafletMonkeyPatch.js')),
    #tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
    fluidRow(
      column(width = 8,
        box(width = 910,height = 900, solidHeader = TRUE,uiOutput('plot_view', height =870, width = 900))
        ),
      column(width = 4,
             box(width = NULL, status = "warning",
                 uiOutput("municSelector")
             ),
             box(width = 475, status = "warning",
                 uiOutput("factor_chooser")#,
                #selectInput("select_factor1", translator$t("Simulation Factor"), choices = factorList()
                #),
                #selectInput("select_factor2", "Simulation Factor",
                #            colnames(ponding_df),
                #            selected = 12
                #),
                #selectInput("select_factor3", "Simulation Factor",
                #            colnames(ponding_df),
                #            selected = 5
                #)
                  #    selectInput('selected_language',
                  #               "Language",
                   #             choices = list("English" = "en", "Espanol" = "es"))
                 #     checkboxGroupInput("directions", "Show",
                 #                        choices = c(
                 #                          Northbound = 4,
                 #                          Southbound = 1,
                 #                          Eastbound = 2,
                 #                          Westbound = 3
                 #                        ),
                 #                        selected = c(1, 2, 3, 4)
                 #     ),
                 #     p(
                 #       class = "text-muted",
                 #       paste("Note: a route number can have several different trips, each",
                 #             "with a different path. Only the most commonly-used path will",
                 #             "be displayed on the map."
                 #       )
                 #     ),
                 #     actionButton("zoomButton", "Zoom to fit buses")
             ),
             box(width = NULL, status = "warning",
                 #uiOutput("more_info")
                 #     selectInput("interval", "Refresh interval",
                 #                 choices = c(
                 #                   "30 seconds" = 30,
                 #                   "1 minute" = 60,
                 #                   "2 minutes" = 120,
                 #                   "5 minutes" = 300,
                 #                   "10 minutes" = 600
                 #                 ),
                 #                 selected = "60"
                 #     ),
                 #     uiOutput("timeSinceLastUpdate"),
                      actionButton("refresh", "Refresh now")#,
                 #     p(class = "text-muted",
                 #       br(),
                 #       "Source data updates every 30 seconds."
                 #     )
             )
      )#


      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {


  output$tabName <- reactive(paste0('Name: ', input$tabs))



  filteredData <- reactive({

    v.budget = input$select_budget
    v.climate = input$select_climate
    resultsdf <- ponding_df[ which(ponding_df$budget==v.budget) , ]
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


  output$plot=renderPlot({

    v.budget = input$select_budget
    v.climate = input$select_climate
    resultsdf <- ponding_df[ which(ponding_df$budget==v.budget) , ]
    data.for.plot <- resultsdf[ which(resultsdf$climate_scenario ==v.climate) , ]

    cvgeo.split <- t(sapply(data.for.plot$geographic_id, function(x) substring(x, first=c(3), last=c(5))))#Split Municipality Numbers from Census ID
    municipality <- as.numeric(cvgeo.split)
    data.for.plot <- cbind(data.for.plot, municipality) #ADD Municipality Numbers from Census ID
    subsetdf = data.for.plot %>%
      group_by(municipality) %>%
      summarise_all(funs(mean))


    if(length(input$select_factor1) < 1 || length(input$select_factor2) < 1 || length(input$select_factor3) < 1){
       plott <- ggplot(subsetdf, aes(x= household_potable_water_vulnerability, y = scarcity_index, size = resident_income_per_capita)) +
        #ggplot(subsetdf, aes(x= input$selectfactor1, y = input$select_factor2, size = input$select_factor3)) +
        geom_point(shape = 21, colour = "#000000", fill = "#40b8d0")+ geom_text(aes(label=municipality),hjust=0, vjust=0)

    }else{
      #xval <<- subsetdf [[input$select_factor1]]
      #yval <<-  subsetdf [[input$select_factor2]]
      #zval <<-  subsetdf [[input$select_factor3]]
      #subsetdf <- data.frame(xval, yval,zval)
      #test.value <<- input$select_factor1

      #plott <- ggplot(subsetdf, aes(x= xval, y = yval, size = zval)) +
       #plott <- ggplot(subsetdf, aes(x= household_potable_water_vulnerability, y = scarcity_index, size = resident_income_per_capita)) +
      plott <-ggplot(subsetdf, aes_string(x= input$select_factor1, y = input$select_factor2, size = input$select_factor3)) +
        geom_point(shape = 21, colour = "#000000", fill = "#40b8d0") + geom_text(aes(label=municipality),hjust=0, vjust=0)

    }
      plott
  })

  # Reactive expression for the data subsetted to what the user selected
  plotData <- reactive({

    budgets.tested <- unique(ponding_df$budget)

    #Iterate through each Budget test and get the average value for the parameter selected
    values <- NULL
    for (val in budgets.tested){
      subsetdf = load_scenario(cache, budget == val)
      cvgeo.split <- t(sapply(subsetdf$cvgeo, function(x) substring(x, first=c(8), last=c(9))))#Split Municipality Numbers from Census ID
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


      avg <- mean(subsetdf[[input$tabs]], na.rm = TRUE)
      values <- c(values, avg)
      values
    }
    #adjust the budget value to show the fraction of the total census blocks

    plot.df <- data.frame(budgets.tested,values)
    plot.df

  })


  output$plot_view <- renderUI({
    plotOutput("plot", width = 800)
  })

  output$municSelector <- renderUI({
    selectInput("select_municipality", translator$t("Municipality"), choices = municipalityList(), width = 200
    )
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
    #climate.tested


    as.list(climate.tested)
  })

  budgetList <- reactive({

    pcts.tested <- as.integer((budgets.tested  / 2428) * 100) #2428 Is the total number of cenusus units, or AGEB_ID
    pcts.tested <- as.character(pcts.tested)
    pcts.tested <- paste(pcts.tested, "%", sep="")
    names(budgets.tested) <- pcts.tested
    as.list(budgets.tested)
  })

  municipalityList <- reactive({
    places = list(translator$t("All municipalities"), "Azcapotzalco", "Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero", "Iztacalco",
                  "Iztapalapa","La Magdalena Contreras","Milpa Alta","Álvaro Obregón","Tláhuac","Tlalpan",
                  "Xochimilco","Benito Juárez","Cuauhtémoc","Miguel Hidalgo","Venustiano Carranza")
    listvalues = (1:17)
    names(listvalues) <- places
    as.list(listvalues)

  })

  factorName <- reactive({

    if(length(input$select_factor) < 1){
      name.of.factor = translator$t("Vulnerability of residents to potable water scarcity")
    }
    else{
      if (identical(input$select_factor, "potable_water_vulnerability_index")){name.of.factor = translator$t("Vulnerability indicator of residents to potable water scarcity")}
      if (identical(input$select_factor, "non_potable_water_vulnerability_index")){name.of.factor = translator$t("Vulnerability indicator of residents to flooding")}
      if (identical(input$select_factor, "potable_water_system_intervention_count")){name.of.factor = translator$t("Number of Actions on Potable Water Infrastructure")}
      if (identical(input$select_factor, "non_potable_water_system_intervention_count")){name.of.factor = translator$t("Number of Actions on Sewer and Drainage Infrastructure")}
      if (identical(input$select_factor,"potable_water_infrastructure_age")){name.of.factor = translator$t("Potable water infrastructure age")}
      if (identical(input$select_factor, "non_potable_water_infrastructure_age")){name.of.factor = translator$t("Non Potable water infrastructure age")}
      if (identical(input$select_factor,"days_no_potable_water")){name.of.factor = translator$t("Days without potable water")}
    }
    name.of.factor

  })

  factorList <- reactive({

    # factor.names = list(translator$t("Vulnerability indicator of residents to potable water scarcity"),translator$t("Vulnerability indicator of residents to flooding"),translator$t("Number of Actions on Potable Water Infrastructure"),
    #                     translator$t("Number of Actions on Sewer and Drainage Infrastructure"),translator$t("Potable water infrastructure age") ,translator$t("Non Potable water infrastructure age"),translator$t("Days without potable water"))
    # choices = list("potable_water_vulnerability_index","non_potable_water_vulnerability_index", "potable_water_system_intervention_count","non_potable_water_system_intervention_count",
    #                "potable_water_infrastructure_age","non_potable_water_infrastructure_age", "days_no_potable_water")

    factor.names = list(translator$t("Vulnerability"),translator$t("Resilience"),translator$t("Sensitivity"))
    choices = list("vulnerability","resilience", "sensitivity")

    names(choices) <- factor.names
    as.list(choices)

  })

  output$factor_chooser<- renderUI({

    tagList(
      selectInput("select_factor1", translator$t("Simulation Factor"), choices = factorList(), selected = "vulnerability"
      ),
      selectInput("select_factor2", translator$t("Simulation Factor"), choices = factorList(), selected = "resilience"
      ),
      selectInput("select_factor3", translator$t("Simulation Factor"), choices = factorList(), selected = "sensitivity"
      )
      #selectInput("select_municipality", translator$t("Municipality to Display"), choices = municipalityList()
      #),
      #selectInput("select_budget", translator$t("Budget Scenarios"), choices = budgetList()
      #),
      #plotOutput("plot")
    )
  })



}

# Run the application
shinyApp(
  server = server,
  ui = ui,
  options = list(port = "5601", host = "0.0.0.0")
)
