library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

# Load and Filter Data
grad_surv <- read_csv("graduate_survey.csv") %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, WebFramework, Industry, AISearch, AITool, Employment)
# View data set
View(grad_surv)

# Handle missing values
# Convert blanks to NA
grad_surv[grad_surv == ""] <- NA  
# Remove rows with NA values
grad_surv <- grad_surv[complete.cases(grad_surv), ]  

# Get Top 5 Campuses (by count)
top5_campuses <- grad_surv %>%
  count(Campus, sort = TRUE) %>%
  slice_max(n, n = 5)
View(top5_campuses)

# Filter full dataset for only the top 5 campuses
Top5CampusWithData <- grad_surv %>%
  filter(Campus %in% top5_campuses$Campus)
View(Top5CampusWithData)

# UI Section
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Eduvos IT Graduate Survey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Programming Languages", tabName = "languages", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "frameworks", icon = icon("globe")),
      menuItem("Cloud Platforms", tabName = "cloud", icon = icon("cloud")),
      menuItem("AI Tools", tabName = "ai", icon = icon("robot")),
      menuItem("Employment Trends", tabName = "employment", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "languages", 
              fluidRow(
                box(title = "Programming Languages - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("langBarPlot", height = "400px")),
                box(title = "Programming Languages - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("langPieChart", height = "400px"))
              )
      ),
      tabItem(tabName = "databases", 
              fluidRow(
                box(title = "Database Usage - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("dbBarPlot", height = "400px")),
                box(title = "Database Usage - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("dbPieChart", height = "400px"))
              )
      ),
      tabItem(tabName = "frameworks", 
              fluidRow(
                box(title = "Popular Web Frameworks - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("frameworkBarPlot", height = "400px")),
                box(title = "Popular Frameworks - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("frameworkPieChart", height = "400px"))
              )
      ),
      tabItem(tabName = "cloud", 
              fluidRow(
                box(title = "Cloud Platform Preferences - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("cloudBarPlot", height = "400px")),
                box(title = "Cloud Platform Preferences - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("cloudPieChart", height = "400px"))
              )
      ),
      tabItem(tabName = "ai", 
              fluidRow(
                box(title = "AI Tools Usage - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("aiBarPlot", height = "400px")),
                box(title = "AI Tools Usage - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("aiPieChart", height = "400px"))
              )
      ),
      tabItem(tabName = "employment", 
              fluidRow(
                box(title = "Employment Status - Bar Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("employmentBarPlot", height = "400px")),
                box(title = "Employment Status - Pie Chart", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("employmentPieChart", height = "400px"))
              )
      )
    )
  )
)

# Server Section
server <- function(input, output) {
  output$langBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined languages into separate rows using ";" as delimiter
      separate_rows(ProgLang, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(ProgLang)) %>%  
      # Count each language occurrence
      count(ProgLang) %>%
      # Top 10 most Popular Programming Languages
      slice_max(n, n = 10) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "Most Popular Programming Languages", 
           x = "Languages", 
           y = "Count") +
      theme_minimal()
  })
  output$langPieChart <- renderPlot({
    # Split the languages into separate rows and count occurrences
    lang_counts <- Top5CampusWithData %>%
      separate_rows(ProgLang, sep = ";") %>%
      filter(!is.na(ProgLang)) %>%
      count(ProgLang)
    # Create a pie chart using coord_polar
    ggplot(lang_counts, aes(x = "", y = n, fill = ProgLang)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of Programming Languages")
  })
  output$dbBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined databases into separate rows using ";" as delimiter
      separate_rows(Databases, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(Databases)) %>%
      # Count each database occurrence
      count(Databases) %>%
      # Top 10 most used Databases
      slice_max(n, n = 10) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(Databases, n), y = n, fill = Databases)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "Database Usage", 
           x = "Databases", 
           y = "Count") +
      theme_minimal()
  })
  output$dbPieChart <- renderPlot({
    # Split the databases into separate rows and count occurrences
    db_counts <- Top5CampusWithData %>%
      separate_rows(Databases, sep = ";") %>%
      filter(!is.na(Databases)) %>%
      count(Databases)
      View(db_counts)
    # Create a pie chart using coord_polar
    ggplot(db_counts, aes(x = "", y = n, fill = Databases)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of Database Usage")
  })
  output$frameworkBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined frameworks into separate rows using ";" as delimiter
      separate_rows(WebFramework, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(WebFramework)) %>%
      # Count each framework occurrence
      count(WebFramework) %>%
      # Top 10 most Popular Web Frameworks
      slice_max(n, n = 10) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "Popular Web Frameworks", 
           x = "frameworks", 
           y = "Count") +
      theme_minimal()
  })
  output$frameworkPieChart <- renderPlot({
    # Split the frameworks into separate rows and count occurrences
    framework_counts <- Top5CampusWithData %>%
      separate_rows(WebFramework, sep = ";") %>%
      filter(!is.na(WebFramework)) %>%
      count(WebFramework)
    # Create a pie chart using coord_polar
    ggplot(framework_counts, aes(x = "", y = n, fill = WebFramework)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of Web Frameworks")
  })
  output$cloudBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined platforms into separate rows using ";" as delimiter
      separate_rows(Platform, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(Platform)) %>%
      # Count each platform occurrence
      count(Platform) %>%
      # Top 10 most Cloud Platform Preferences
      slice_max(n, n = 10) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(Platform, n), y = n, fill = Platform)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "Cloud Platform Preferences", 
           x = "Cloud Platforms", 
           y = "Count") +
      theme_minimal()
  })
  output$cloudPieChart <- renderPlot({
    # Split the platforms into separate rows and count occurrences
    cloud_counts <- Top5CampusWithData %>%
      separate_rows(Platform, sep = ";") %>%
      filter(!is.na(Platform)) %>%
      count(Platform)
    # Create a pie chart using coord_polar
    ggplot(cloud_counts, aes(x = "", y = n, fill = Platform)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of Cloud Platform Preferences")
  })
  output$aiBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined ai tools into separate rows using ";" as delimiter
      separate_rows(AITool, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(AITool)) %>%
      # Count each ai tool occurrence
      count(AITool) %>%
      # Top 10 most used AI TOOls 
      slice_max(n, n = 10) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(AITool, n), y = n, fill = AITool)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "AI Tools Usage", 
           x = "AI Tools", 
           y = "Count") +
      theme_minimal()
  })
  output$aiPieChart <- renderPlot({
    # Split the ai tools into separate rows and count occurrences
    tool_counts <- Top5CampusWithData %>%
      separate_rows(AITool, sep = ";") %>%
      filter(!is.na(AITool)) %>%
      count(AITool)
    # Create a pie chart using coord_polar
    ggplot(tool_counts, aes(x = "", y = n, fill = AITool)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of AI Tools Usage")
  })
  output$employmentBarPlot <- renderPlot({
    Top5CampusWithData %>%
      # Split the combined employment status into separate rows using ";" as delimiter
      separate_rows(Employment, sep = ";") %>%
      # Remove NA values before plotting
      filter(!is.na(Employment)) %>%
      # Count each employment status occurrence
      count(Employment) %>%
      # Create the bar plot
      ggplot(aes(x = reorder(Employment, n), y = n, fill = Employment)) +
      geom_bar(stat = "identity") +
      
      coord_flip() +
      labs(title = "Employment Status", x = "Employment", y = "Count") +
      theme_minimal()
  })
  output$employmentPieChart <- renderPlot({
    # Split the employment status into separate rows and count occurrences
    employment_counts <- Top5CampusWithData %>%
      separate_rows(Employment, sep = ";") %>%
      filter(!is.na(Employment)) %>%
      count(Employment)
    # Create a pie chart using coord_polar
    ggplot(employment_counts, aes(x = "", y = n, fill = Employment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Pie Chart of Employment Status")
  })
}

# Run the App
shinyApp(ui = ui, server = server)
