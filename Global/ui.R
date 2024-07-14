#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



#--------------------------------ui.R--------------------------------
library(shiny)

library(shinydashboard)

dashboardPage(
  skin = "black",
  #------------- Title-----------
  dashboardHeader(
    title = "Energy Analysis"
  ),
  
  
  
  #-------------- Menu -------------
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dashboard", tabName = "page1", icon = icon("earth")),
    menuItem("Graph", tabName = "page2", icon = icon("chart-simple")),
    menuItem("Map", tabName = "page3", icon = icon("map")),
    menuItem("Data", tabName = "page4", icon = icon("th"))
  )
),
  
  
  #------------- Isi ------------ 
  dashboardBody(
    tabItems(
#-----------Dashboard ---------------
      tabItem(
        tabName = "page1",
        fluidRow(
          infoBox("Record",
                  value = nrow(data2),
                  icon = icon("earth"),
                  width = 4,
                  color = "blue"),
          infoBox("Country",
                  value = length(unique(data$Country)),
                  icon = icon("city"),
                  width = 4,
                  color = "blue"),
          infoBox("Time Series",
                  value = length(unique(data$Year)),
                  icon = icon("clock"),
                  width = 4,
                  color = "blue")
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput(outputId = "plot1")
          )
        )
      ),
# Add additional tabItems as needed
      
#-----------menu 1 ---------------
tabItem(
  tabName = "page2",
  fluidRow(
    box(
      width = 12,
      selectInput(
        inputId = "Category",
        label = "Input Category",
        choices = unique(data2$Category),
        selected = "Total energy consumption"
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      plotlyOutput(outputId = "plot2")
    ),
    box(
      width = 12,
      plotlyOutput(outputId = "plot3")
    )
  )
),
#-------------menu2-----------
tabItem(
  tabName = "page3",
  fluidRow(
    box(
      width = 9,
      radioButtons(
        inputId = "Category",
        label = "Category",
        choices = unique(data$Category),  # Assuming 'Country' is a column in your data
        selected = "Total energy production",          # Default selected country
        inline = TRUE                    # Display buttons inline (optional)
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      plotlyOutput(outputId = "plot4")
    )
  )
),

#-------------menu3-----------
tabItem(
  tabName = "page4",
  fluidRow(
    box(
      width = 12,
      title = "Dataset Energy Statistical",
      dataTableOutput(outputId = "data")
    )
  )
))))
