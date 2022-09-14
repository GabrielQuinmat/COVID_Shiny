#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(semantic.dashboard)
library(plotly)
library(leaflet)
library(reactable)
library(lubridate)

homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Country Level", class = "tab-content__title"),
    fluidRow(
      box(
        title = "Acumulated Cases Through Time",
        collapsible = F, ribbon = F, title_side = "top",
        plotlyOutput(ns("totalHistoric")),
        width = 16
      )
    ),
    fluidRow(
      box(
        title = "Acumulated Cases Today",
        collapsible = F, ribbon = F, title_side = "top",
        leafletOutput(ns("totalMap"), width = "100%", height = 600)
      ),
      box(
        title = "Top States with Higher Amount of Cases",
        collapsible = F, ribbon = F, title_side = "top",
        reactableOutput(ns("totalTable"), width = "100%", height = 600)
      )
    )
  )
}

stateUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("State Level", class = "tab-content__title"),
    selectInput(ns("stateSelect"), "Select a State", 
    c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", 
    "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG",
    "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO",
    "DF"), selected="RO"),
    fluidRow(
      box(
        title = "Acumulated Cases Today",
        collapsible = F, ribbon = F, title_side = "top",
        width = 16,
        leafletOutput(
          ns("stateMap"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      box(
        title = "Counties with Higher Amount of Cases",
        collapsible = F, ribbon = F, title_side = "top",
        reactableOutput(ns("stateTable"), width = "100%", height = 600),
        width = 16
      )
    )
  )
}


ui <- dashboardPage(
  sidebar_and_body_container_class = "class2",
  dashboardHeader(title = "Brazil COVID Cases"),
  dashboardSidebar(
    class = "dash-sidebar",
    sidebarMenu(
      menuItem(
        tabName = "home",
        text = "General",
        icon = icon("home")
      ),
      menuItem(
        tabName = "state",
        text = "State",
        icon = icon("landmark")
      )
    )
  ),
  dashboardBody(
    class = "asdasd",
    tabItems(
      tabItem(
        tabName = "home",
        homeUI("homeTab")
      ),
      tabItem(
        tabName = "state",
        stateUI("stateTab")
      )
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  )
)

# Define UI for application that draws a histogram
shinyUI(ui)
