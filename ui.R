# inspo: https://github.com/ronammar/baby_names_shiny_appy
# state and territory data here: https://www.ssa.gov/oact/babynames/limits.html

library(ggnewscale)
library(viridis) 
library(sf)
library(tidyverse)
library(ggrepel)
library(fresh)
library(shiny)
library(shinydashboard)
library(usmap)


babyNames_st <- readRDS("baby_names_st.rds")
babyNames <- readRDS("baby_names.rds")
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

#icon list:
#https://fontawesome.com/icons

# fresh: https://github.com/dreamRs/fresh
## custom theme party
# mytheme <- create_theme(
#   adminlte_color(
#     light_blue = "#ACBCCC"
#   ),
#   adminlte_sidebar(
#     width = "400px",
#     dark_bg = "#D8DEE9",
#     dark_hover_bg = "#81A1C1",
#     dark_color = "#2E3440"
#   ),
#   adminlte_global(
#     content_bg = "#FFF",
#     box_bg = "#D8DEE9", 
#     info_box_bg = "#D8DEE9"
#   )
# )


#####################################
############ UI #################
#####################################
ui <- dashboardPage( 
  dashboardHeader( title = "Baby Names"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview Data", tabName = "overview", icon = icon("dashboard")),
      menuItem("Yearly Data (national)", tabName = "year", icon = icon("calendar")),
      menuItem("Least Common Names (national)", tabName = "table", icon = icon("table")),
      menuItem("Popularity by State", tabName = "maps", icon = icon("map"))
    )),
  dashboardBody(
    #use_theme(mytheme),
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
              textInput("names", "Lookup names (comma separated):",
                        value="Taylor, Travis, Jim, Pam, Dwight"),
              plotOutput("density"),
              width=11),
      
      # Second tab content
      tabItem(tabName = "year",
              # Input: Slider for the number of bins ----
              fluidPage(
                h3("Select a year for name data"),
                sliderInput("year", "Birth year",
                            min=min(babyNames$year),
                            max=max(babyNames$year), value = 1989)),
              plotOutput("hist"),
              renderText("Least popular names (selection)"),
              plotOutput("histLeast"),
              
      ), #end tabItem
      tabItem(tabName = "table",
              fluidPage(  fluidRow(h2("Select a year for name data (National-level)") ),
                          fluidRow( 
                            column(8, sliderInput("yearT", "Birth year",
                                                  min=min(babyNames$year),
                                                  max=max(babyNames$year), value = 1989)),
                            column( 4, radioButtons("gender", "Which gender of names do you want included?", c("M", "F")))),
                          dataTableOutput('table'))
      ),
      tabItem(tabName = "maps",
              fluidPage(
                h3("Geographic popularity: State-by-state"),
                fluidRow( 
                  column(8, sliderInput("yearM", "Birth year",
                                        min=min(babyNames_st$year),
                                        max=max(babyNames_st$year), value = 1989)),
                  column( 4, radioButtons("sexM", "Select a gender", c("M", "F")))),
                plotOutput("name_map")
              ))
    )
  )
)

