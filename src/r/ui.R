#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
require(shinydashboard)
library(leaflet)
library(DT)
#######################################################################################################
navbarPage(
  title = "MEGADAPT APP",
  # Application title

  mainPanel(
    img(
      src = "MEGADAPT_picture_inicio.png",
      height = 600,
      width = 1260,
      align = "Center"
    ),
    theme = shinytheme("lumen")
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Introduction"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(
          menuItem("The Project", tabName = "intro_megadapt", icon = icon("question-circle")),
          menuItem("The Model", tabName = "intro_model", icon = icon("file-code", class = "far")),
          menuItem("Flow diagram", tabName = "flow_diag", icon = icon("project-diagram", class = "far"))
        ),
        tags$div(
          actionButton("Iniciar", inputId = "setup")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "intro_megadapt",
            fluidPage(
              "Megacities are hotspots of climate change vulnerability and face significant social and institutional challenges to adaptation. The MEGADAPT project will address the challenge of reducing vulnerability under climate change to increased flooding, chronic water scarcity, and associated health outcomes in Mexico City, one of the world's largest metropolitan areas. Residents, businesses and public agencies in megacities respond to the impact of flooding, scarcity and health outcomes in disparate and often uncoordinated ways across the metropolitan area. Their responses impact the hydro-climatic system that generates hazardous conditions through their modifications of the built environment and the biophysical landscape. The project produces an integrated dynamic model - MEGADAPT - for use in Mexico City, but with applicability to climate risk adaptation in complex urban environments across the globe. The project explores how different scenarios of changes in climatic extremes combine with dispersed actions of specific populations in response to vulnerability (e.g., changes in land use or infrastructure) to produce cross-scalar feedbacks that alter the distribution of vulnerability in the megacity. As a decision-support tool, MEGADAPT enables decision-makers to test how altering risk management priorities or the geographic focus of interventions under changing climatic conditions affect social equity and overall risk outcomes."
            )
          ),
          tabItem(
            tabName = "intro_model",
            fluidPage(
              "This document describes the implementation of the agent-based model of the MEGADAPT project (adaptation in a megacity). The full MEGADAPT model simulates the coupling between biophysical processes and the decisions of residents and the water authority of Mexico City. The aim of the model is to investigate the consequences of this coupling for the spatial distribution of socio-hydrological vulnerability in Mexico City. 
                                   The agent-based model presented here simulates the decision-making processes of different socio-institutional and socio-political actors of Mexico City, and their actions and decisions to adapt to hydrological risk and vulnerabilities. 
                                  The model is divided in three modules that together they incorporate a set of procedures to simulate the socio-hydrological vulnerability in a Megacity. These modules are: The socio-Institutional module, the risk module and the socio-political module. The three modules represent different actors and agents that interact internally and with other agents from the other modules.
                                  The socio-institutional module represents the decisions of socio-institutional agents to invest in infrastructure systems associated to water management. In this module, the decision-making procedures that simulate the decisions are built using multi-criteria decision techniques. 
                                  The socio-political module represents the action of resident and other local stakeholder actors that can influence the decisions of socio-institutional actors. 
                                  Finnaly, the risk module simulates events of exposure. Within the module, frequency-based models simulate the exposure. These models are parametrized based on empirical information that associate the frequency of the events to the condition of the infrastructure systems that are modify by the socio-institutional agents. Socio-political actors such a as resident suffer the exposure that modify their decision based on the level of exposure. The actions of the socio-political agents influence the condition and supply of infrastructure that then should modify the condition of the infrastructure and therefore the risk. The landscape is divided in spatial units, each of them, which confines a unit of risk where the frequency of events are accumulated, and the actions are implemented. Each spatial unit is represented by a set of attributes that represent the condition of the infrastructure systems and the population. Based on the values of the attributes, 
                                  frequency based models simulate water scarcity, flooding and health hazard.
                                  "
            )
          ),
          tabItem(
            tabName = "flow_diag",
            fluidPage(
              box(img(
                src = "diagram_model.png",
                height = 700,
                width = 900,
                align = "Center"
              ), width = 8),
              box(fluidPage("label"))
            )
          )
        )
      ),
      skin = "purple"
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Study Area"),
    dashboardPage(
      dashboardHeader(title = "Mexico City"),
      dashboardSidebar(),
      dashboardBody(
        box(leafletOutput("map"), width = 12),
        box("label")
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Socio-Institutional Module"),
    dashboardPage(
      dashboardHeader(title = "Mental Models"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Introduction", tabName = "Intro"),
          menuItem("Water Authority", tabName = "SACMEX"),
          menuItem("Residents", tabName = "Resid_MM")
        ),
        tabItems(
          tabItem(
            tabName = "SACMEX",
            selectInput(
              "criteria1", "Criteria1:",
              list(
                "Abastecimiento" = "Abast",
                "Antiguedad Ab" = "Antiguedad_Ab",
                "Antiguedad D" = "Antiguedad_D",
                "Basura" = "basura",
                "Capacidad drenaje" = "Capacidad_D",
                "Capacidad red distribucion" = "Capacidad_Ab",
                "Calidad_agua" = "cal_agua",
                "Encharcamientos" = "encharcamientos",
                "Escasez de agua" = "water_scarcity",
                "Escurrimiento" = "Escu",
                "Falla" = "gear",
                "Falta" = "falta",
                "Falta" = "falta",
                "Falta" = "falta",
                "Gasto-hidraulico" = "gast_hy",
                "Hundimientos" = "hund",
                "Innundaciones" = "flood",
                "Monto" = "monto",
                "Peticiones Delegacionales" = "per_del",
                "Peticiones de Usuarios" = "pet_deU",
                "Precipitaciones" = "rain",
                "Presion Hidraulica" = "Presion Hidraulica",
                "Presion de medios" = "pres_de_med",
                "Presion social" = "social_pressure"
              )
            ),

            tags$div(
              selectInput(
                "criteria2", "Criteria2:",
                list(
                  "Abastecimiento" = "Abast",
                  "Antiguedad Ab" = "Antiguedad_Ab",
                  "Antiguedad D" = "Antiguedad_D",
                  "Basura" = "basura",
                  "Capacidad drenaje" = "Capacidad_D",
                  "Capacidad red distribucion" = "Capacidad_Ab",
                  "Calidad_agua" = "cal_agua",
                  "Encharcamientos" = "encharcamientos",
                  "Escasez de agua" = "water_scarcity",
                  "Escurrimiento" = "Escu",
                  "Falla" = "gear",
                  "Falta" = "falta",
                  "Falta" = "falta",
                  "Falta" = "falta",
                  "Gasto-hidraulico" = "gast_hy",
                  "Hundimientos" = "hund",
                  "Innundaciones" = "flood",
                  "Monto" = "monto",
                  "Peticiones Delegacionales" = "per_del",
                  "Peticiones de Usuarios" = "pet_deU",
                  "Precipitaciones" = "rain",
                  "Presion Hidraulica" = "Presion Hidraulica",
                  "Presion de medios" = "pres_de_med",
                  "Presion social" = "social_pressure"
                )
              )
            )
          ),
          tabItem(tabName = "Resid_MM")
        )
      ),
      dashboardBody(
        DTOutput("mytable")
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Site Suitability"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        plotOutput("plot2", width = 1000, height = 1000)
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Site selection"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sliderInput(
          inputId = "budget", label = "Presupuesto",
          min = 240, max = 2400,
          value = 750
        ),
        actionButton(inputId = "site_selection_run", label = "find best sites")
      ),
      dashboardBody(
        plotOutput("plot4")
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Risk Module"),
    dashboardPage(
      dashboardHeader(title = "Models"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Flooding", tabName = "flood", icon = icon("umbrella")),
          menuItem("Scarcity", tabName = "sinAgua", icon = icon("tint")),
          menuItem("Health", tabName = "salud", icon = icon("medkit"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "flood",
            fluidRow(
              box(plotOutput(outputId = "flooding_map"))
            )
          ),
          tabItem(
            tabName = "sinAgua",
            box(
              plotOutput(outputId = "scarcity_map")
            )
          )
        )
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Run Simulation"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        actionButton("setup_sim", "iniciar"),
        actionButton("Go", "Run_simulation"),
        sliderInput(inputId = "anios", label = "tiempo de simulacion", min = 1, max = 40, value = 1, step = 1)
      ),
      dashboardBody(
        plotOutput("plot3")
      )
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Results"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        selectInput(
          "Resultado_pick", "Scale:",
          list("Census block", "Delegation", "City")
        ),
        selectInput(
          "dimension", "dimension",
          list("Spatial (Maps)", "Temporal (Time-series)")
        ),
        selectInput(
          "Ind_var", "variable",
          list("")
        )
      ),
      dashboardBody()
    )
  ),
  #######################################################################################################
  tabPanel(
    tags$h2("Contact"),
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody()
    )
  )
)
#######################################################################################################
