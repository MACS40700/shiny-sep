#####################################
############ SERVER #################
#####################################
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
# set up plotting
# Histogram plotter
makeHistogram <- function(inputYear, topNum = 10, topNames=TRUE) {
  # Plot a histogram of name counts for the top names for a given year
  d <- babyNames %>%
    mutate(name=str_to_title(name)) %>%
    filter(year == inputYear) %>%
    group_by(sex)
  
  if (topNames) {
    d <- d %>% 
      arrange(desc(count)) %>%
      top_n(topNum, count)
  } else {  # bottom
    d <- d %>% 
      arrange(count) %>%
      top_n(-topNum, count) %>%
      # there are more than 20 with ties for lowest count, sample 20 random
      sample_n(topNum)
  }
  
  # Below we use custom factor levels to preserve ordering when plotted
  d$name <- factor(d$name, levels=d$name)
  
  ggplot(d, aes(x=name, y=count, fill=sex) ) +
    facet_wrap(~ sex, scale="free") +
    geom_bar(color = "#81A1C1", stat="identity") +
    labs(x="Baby name", y="Number of babies", fill="Sex") +
    scale_fill_manual(values = c(M = "white", F = "#ACBCCC")) + 
    theme_bw(17) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
}

server <- function(input, output) { 
  
  ##### DENSITY PLOT #########
  output$density <- renderPlot({
    # Density plot of specific name popularity over the years
    specificNames <- unlist(str_split(str_to_lower(input$names), ","))
    # Trim any whitespace between names
    specificNames <- str_trim(specificNames)
    
    d <- filter(babyNames, name %in% specificNames) %>% 
      group_by(name, sex) %>%
      mutate(label = if_else(count == max(count), as.character(paste(name, sex, sep = ", ")), NA_character_),
             name_sex =as.character(paste(name, sex, sep = ", "))) # separate lines for each name/sex
    
    ggplot(d) +
      #facet_wrap(~ sex, scales="free", nrow = 2) +
      geom_line(aes(x=year, y=count, color=name_sex),filter(d, sex == "F"), linewidth=1.5, linetype = 6) + 
      geom_label_repel(aes(x=year, y=count, color=name_sex, label = label),filter(d, sex == "F"), show.legend  = FALSE,
                       nudge_x = 1,  nudge_y = 1, na.rm = TRUE) +
      scale_color_viridis_d(option = "magma", end = 0.8) +
      labs(colour = "Female names") +
      new_scale_colour() +  
      geom_line(aes(x=year, y=count, color=name_sex), filter(d, sex == "M"), linewidth=1.5, linetype = 1) + 
      geom_label_repel(aes(x=year, y=count, color=name_sex, label = label), filter(d, sex == "M"), show.legend  = FALSE,
                       nudge_x = 1,  nudge_y = 1, na.rm = TRUE) +
      labs(colour = "Male Names") +
      scale_color_viridis_d(option = "magma", end = 0.8, alpha = 0.8) +
      theme_bw(15)
  })
  
  ######## HIST NAMES/YEAR ##########
  output$hist <- renderPlot({
    makeHistogram(input$year)
  })
  
  output$histLeast <- renderPlot({
    makeHistogram(input$year, topNames=FALSE)})
  
  ## NAMES TABLE ##
  output$table <- renderDataTable(
    babyNames %>%
      mutate(name=str_to_title(name)) %>% 
      filter(year == input$yearT) %>%
      filter(sex == input$gender) %>% 
      group_by(sex) %>%
      arrange(count) %>%
      top_n(-15, count),
    options = list(pageLength = 50))
  
  output$table_st <- renderDataTable(
    babyNames_st %>% filter(state == st) %>%
      mutate(name=str_to_title(name)) %>% 
      filter(year == input$yearT) %>%
      group_by(sex) %>%
      arrange(count) %>%
      top_n(-15, count),
    options = list(pageLength = 50))
  
  
  
  output$name_map <- renderPlot({
    
    bn_sub <- babyNames_st %>% 
      filter(year == input$yearM & sex == input$sexM) %>% group_by(state) %>%
      mutate(label = if_else(count == max(count), as.character(name), NA_character_)) %>%
      filter( !is.na(label))# %>%  
    
    left_join(usa, bn_sub,  by = c("ID"="state_name")) %>% #need to merge in this order for geometry
      ggplot() +
      geom_sf(aes(fill = label, color = label)) +
      geom_sf_text(aes(label = label), size = 3.5, color = "white") + 
      theme_bw() + scale_color_viridis_d(end = 0.9, option = "magma") + scale_fill_viridis_d(end = 0.9, option = "magma")
    
    
  })
}