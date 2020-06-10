library(shinythemes)
library(plotly)

ui = fluidPage(theme = shinytheme("superhero"),
  # Application title
  titlePanel("Citibikes for NYC"),
  tabsetPanel(
    tabPanel("Summary",
      sidebarLayout(
        sidebarPanel(
          selectInput('type', 'Choose Your Role:',
                      choices = c('Manager','Customer', 'Model')),
          
          numericInput("id", "Input One Station ID:", 3522),
          sliderInput("No.obs", "No. of obs:",
                      min = 0, max = 2000,
                      value = 500),
          actionButton("update", "Update View"),      
          helpText("The customer focuses on the information of available bikes and docks.The manager focuses on the information of disabled bikes and docks.The No.obs control the number of observations in modeling and plotting. And the stations id is used for customer to find out corresponding information about selected station.")
        ),
        mainPanel(
          # Output: Header + summary of distribution ----
          h4("Description plots and Least weighted regression curves"),
          plotOutput("map"),
          # Output: Header + table of distribution ----
          h4("Summary"),
          tableOutput('view')
        )
      )
    ),
  tabPanel(title = "Scatterplot", 
           mainPanel(
             plotlyOutput("scatterplot")
           ))
  )
)
