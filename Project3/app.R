library(shiny)
library(readxl)
library(knitr)
library(tidyverse)
library(shinyjs)

#initial tasks
fantasyData <- read_excel("fantasyData.xlsx")
fantasyData <- na.omit(fantasyData)
choicesList = list(
  "Team" = "team",
  "Position" = "pos",
  "Games Played" = "games",
  "Fantasy Points Per Game" = "fPts/g",
  "Total Fantasy Points" = "totalfPoints"
)

#UI
ui <- fluidPage(
  titlePanel("Fantasy Football Interactive Dashboard (2022-2023 season)"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      radioButtons("variable", label = h3("Choose a variable"),
                   choices = list(
                     "Team" = "team",
                     "Position" = "pos",
                     "Games Played" = "games",
                     "Fantasy Points Per Game" = "fPts/g",
                     "Total Fantasy Points" = "totalfPoints"
                   ), 
                   selected = "team"),
      hr(),
      sliderInput("slider1", label = h3("Filter data according to player rank"), min = 1, 
                  max = 300, value = 300),
      hr(),
      checkboxGroupInput("checkGroup", label = h3("Filter data according to position"), 
                         choices = list("Wide Reciever" = "WR", "Running Back" = "RB", "Quarterback" = "QB", "Tight End" = "TE", "Fullback" = "FB"),
                         selected = c("WR", "RB", "QB", "TE", "FB")),
      hr(),
      selectInput("select", label = h3("Select color of graph"), 
                  choices = list("Red" = "red", "Green" = "green", "Blue" = "blue"), 
                  selected = 1),
      
      hr(),
      imageOutput("myImage"),
      hr(),
    ),
    mainPanel(
      plotOutput("visualization"),
      verbatimTextOutput("stats"),
      tags$a(href = "https://www.pff.com/news/fantasy-football-cheat-sheet-nfl-week-1-start-sit-advice-pff-rankings-matchups-to-target-and-more", "Photo Source")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    # Filter the data based on total fantasy points and position
    filtered_data <- filter(fantasyData, rank <= input$slider1)
    selected_positions <- input$checkGroup
    filtered_data <- filter(filtered_data, pos %in% selected_positions)
    return(filtered_data)
  })
  
  findColor <- reactive({
    # Color of graph
    return(input$select)
  })
  
  #graph output
  output$visualization <- renderPlot({
    color <- findColor()
    selected_variable <- input$variable
    reverse_mapping <- setNames(names(choicesList), choicesList) 

    #Numeric variables
    if (selected_variable == "games" | selected_variable == "totalfPoints" | selected_variable == "fPts/g") {
      hist(filteredData()[[selected_variable]], 
           main = paste("Distribution of", reverse_mapping[selected_variable]), 
           xlab = 'Value', ylab = "Freq", col = color)
    } else {
      #Categorical variables
      barplot(table(filteredData()[[selected_variable]]), 
              main = paste("Distribution of", reverse_mapping[selected_variable]), 
              xlab = reverse_mapping[selected_variable], ylab = "Freq",
              las = 2, col = color
              )
    }})
  
  #stats output
  output$stats <- renderPrint({
    selected_variable <- input$variable
    if (selected_variable == "games" | selected_variable == "totalfPoints" | selected_variable == "fPts/g"){
      stats <- summary(filteredData()[[selected_variable]])
      stats
    } else {
      table_result <- prop.table(table(filteredData()[[selected_variable]]))
      df <- data.frame(Team = names(table_result), Proportion = as.vector(table_result))
      kable(df)
    }
  })
  
  #image output
  output$myImage <- renderImage({
    list(src = "fantasy-cover.jpg",
         contentType = "image/jpeg",
         width = 225, height = 150, alt = "Fantasy Cover")
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
