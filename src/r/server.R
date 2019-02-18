#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(ggplot2)
require(rgeos)
require(rgdal)
# read setup code
setwd("..")
source("r/setup.R")
# Define server logic required to draw
shinyServer(function(input, output, session) {

  #########################################################################################################################
  # initial setup
  Inicio <- eventReactive(input$Iniciar, {
    studyArea_CVG@data$id <- rownames(studyArea_CVG@data)
    studyArea_CVG.points <- fortify(studyArea_CVG, region = "id")
    studyArea_CVG.df <- join(studyArea_CVG.points, studyArea_CVG@data, by = "id")
    return(studyArea_CVG.df)
  })


  #########################################################################################################################
  output$plot3 <- renderPlot({
    Out_model <- model()


    g1 <- ggplot(Out_model) +
      aes(long, lat, group = group, fill = antiguedad_D) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Age")
    g2 <- ggplot(Out_model) +
      aes(long, lat, group = group, fill = antiguedad_Ab) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Age")

    grid.arrange(g1, g2, ncol = 2)
  }, width = 600, height = 400)

  #########################################################################################################################
  output$plot2 <- renderPlot({
    distance_ideal_A1_D <- site_suitability$distance_ideal_A1_D
    distance_ideal_A2_D <- site_suitability$distance_ideal_A2_D
    distance_ideal_A1_Ab <- site_suitability$distance_ideal_A1_Ab
    distance_ideal_A2_Ab <- site_suitability$distance_ideal_A2_Ab
    Output_value_function <- site_suitability$Output_value_function

    Output_value_function@data$id <- rownames(Output_value_function@data)
    Output_value_function.points <- fortify(Output_value_function, region = "id")
    Output_value_function.df <- join(Output_value_function.points, Output_value_function@data, by = "id")

    G1 <- ggplot(Output_value_function.df, aes(long, lat, group = group, fill = distance_ideal_A1_D)) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Mantenimiento del drenaje")
    G2 <- ggplot(Output_value_function.df, aes(long, lat, group = group, fill = distance_ideal_A2_D)) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Nueva Infra. drainage")
    G3 <- ggplot(Output_value_function.df, aes(long, lat, group = group, fill = distance_ideal_A1_Ab)) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Mantenimiento systema abastecimiento")
    G4 <- ggplot(Output_value_function.df, aes(long, lat, group = group, fill = distance_ideal_A2_Ab)) +
      geom_polygon() +
      coord_equal() +
      scale_fill_continuous("Nueva infra. Abastecimiento")

    grid.arrange(G1, G2, G3, G4, ncol = 2)
  }, height = 1000, width = 1000)

  #########################################################################################################################

  output$mytable <- renderDataTable({
    MCDA_SACMEX_table
  })

  #########################################################################################################################

  example_SS <- eventReactive(input$site_selection_run, {
    Budget <- input$budget
    studyArea_CVG@data$site_selected_example <- numeric(length(studyArea_CVG@data$capac_d))
    studyArea_CVG@data$site_selected_example[order(r)[1:Budget]] <- "Selected"
    studyArea_CVG@data$site_selected_example[order(r)[-c(1:Budget)]] <- "No Selected"
    studyArea_CVG@data$id <- rownames(studyArea_CVG@data)
    studyArea_CVG.points <- fortify(studyArea_CVG, region = "id")
    studyArea_CVG.df <- join(studyArea_CVG.points, studyArea_CVG@data, by = "id")

    return(studyArea_CVG.df)
  })
  #########################################################################################################################
  output$plot4 <- renderPlot({
    outdat <- example_SS()
    ggplot(data = outdat, aes(x = long, y = lat, group = group, fill = site_selected_example)) +
      geom_polygon() +
      coord_equal()
  })
  #########################################################################################################################
  output$flooding_map <- renderPlot({
    outdat <- example_SS()
    ggplot(data = outdat, aes(x = long, y = lat, group = group, fill = encharca)) +
      geom_polygon() +
      coord_equal()
  })
  #########################################################################################################################
  output$scarcity_map <- renderPlot({
    outdat <- example_SS()
    ggplot(data = outdat, aes(x = long, y = lat, group = group, fill = days_wn_water_month)) +
      geom_polygon() +
      coord_equal()
  })

  #########################################################################################################################
  output$map <- renderLeaflet({
    m
  })
  #########################################################################################################################
  # run simulation
  #########################################################################################################################

  eventReactive(input$setup_sim, {
    source("r/setup.R")
  })
  # run simulation
  model <- eventReactive(input$Go, {
    Budget <- input$budget
    withProgress(message = "Simulacion en curso", value = 0, {
      time_run <- 0
      repeat{
        time_run <- time_run + 1
        # Increment the progress bar, and update the detail text.
        incProgress(1 / input$anios, detail = paste("Parte:", time_run))
        source("r/cycle.R", local = T)
        if (time_run == input$anios) {
          break
        }
      }
    })
    studyArea_CVG@data$id <- rownames(studyArea_CVG@data)
    studyArea_CVG.points <- fortify(studyArea_CVG, region = "id")
    studyArea_CVG.df <- join(studyArea_CVG.points, studyArea_CVG@data, by = "id")
    return(studyArea_CVG.df)
  })

  #################################################################################################
  # results
  observe({
    if (input$Resultado_pick == "Por Delegacion") {
      updateSelectInput(session, inputId = "Ind_var", label = "Delegacion", choices = unique(levels(studyArea_CVG@data$municipio)))
    }
    if (input$Resultado_pick == "Por AGEB") {
      updateSelectInput(session, inputId = "Ind_var", label = "AGEB", choices = studyArea_CVG@data$AGEB_ID)
    }
  })
})
