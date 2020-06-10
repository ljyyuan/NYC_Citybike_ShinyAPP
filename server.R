library(dplyr)
library(ggplot2)
library(shiny)
library(jsonlite)
library(shinythemes)
library(plotly)

server = function(input, output){
  
  citibike <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_status.json")

  data = citibike$data
  stations = data$stations
  stations$id = as.numeric(stations$station_id)
  
  #stations = read.csv('stations.csv')
  stations$station_text = paste("station", stations$station_id)
    
  output$scatterplot <- renderPlotly({
    plot_ly(stations, x = ~num_bikes_available, y = ~num_docks_available, text = ~station_text, type = "scatter", mode = "markers", color = ~station_status,
            marker = list(size = ~(num_bikes_disabled+1)*5, opacity = 0.5)) %>%
      layout(title = '',#Scatterplot of Number of Bikes Available against Number of Docks Available',
             xaxis = list(showgrid = FALSE, title = "Number of Docks Available"),
             yaxis = list(showgrid = FALSE, title = "Number of Bikes Available"))
  })
  
  # data = citibike$data
  # stations = data$stations
  stations$id = as.numeric(stations$station_id)

  maptypeInput = eventReactive(input$update, {
    switch(input$type,
           'Manager' = 'Manager',
           'Customer' = 'Customer',
           'Model' = 'Model'
           )
})

  # Generate a summary of the dataset ----
  output$view <- renderTable({
    if(maptypeInput() == 'Model'){
      N = isolate(input$No.obs)
      
      fit = lm(stations$num_bikes_available~ 
                 stations$num_docks_available +
                 stations$num_bikes_disabled + 
                 stations$num_docks_disabled)
      data.frame(Variables = names(fit$coefficients), Coefficients = fit$coefficients)
    }
    else{
      if(maptypeInput() == 'Customer'){
        n = isolate(input$id)
        if(n %in% stations$station_id){
          ind = which(stations$station_id == n)
          D = stations %>% select(c(is_returning,
                                    num_bikes_available,
                                    num_docks_available,
                                    num_bikes_disabled, 
                                    num_docks_disabled, 
                                    is_renting, 
                                    is_installed,
                                    station_status))
          D = D[ind, ]
          D$station_status = as.numeric(D$station_status == 'active')
          vec = unlist(c(D[1,]))
          data.frame(Variables = colnames(D),Values = vec)
        }
        else{
          print("Invalid! Try another station Id!")
        }
      }
      else{D1 = rbind(table(stations$is_installed),
                      table(stations$is_renting),
                      table(stations$is_returning),
                      table(stations$station_status)[2:1])
      D = data.frame(Features = c('Is_installed', 'Is_renting',
                                  'Is_returning', 'Is_active')
      )
      
      D = cbind(D, D1)
      colnames(D) = c('Features', 'No', 'Yes')
      head(D)}
    }
     
  })
  
  output$map <- renderPlot({
    N = isolate(input$No.obs)
    K = isolate(input$id)
    if(N <= 0){
      N = nrow(stations)
    }
    if(N > nrow(stations)){
      N = nrow(stations)
    }
    stations = stations[1:N, ]
    if(maptypeInput() == 'Manager'){

        layout(matrix(1:4, 2, 2, byrow = T))
        hist(stations$num_bikes_available, 
             col = "#75AADB", 
             border = "white",
             xlab = "Number of available bikes in stations",
             main = "Histogram of available bikes")
        
        hist(stations$num_docks_available, 
             col = "#75AADB", 
             border = "white",
             xlab = "Number of available docks in stations",
             main = "Histogram of available docks")
        
        plot(stations$num_bikes_available, 
             stations$num_docks_available, 
             pch = 20, 
             col = 'blue',
             main = 'Not - returning stations',
             xlab = 'Number of available bikes for stations', 
             ylab = 'Number of available docks for stations')
        
        D0 = stations %>% filter(is_returning == 0)
        D1 = stations %>% filter(station_status == 'out_of_service')
        
        points(D0$num_bikes_available,
               D0$num_docks_available, 
               col = 'red', pch = 20)
        points(D1$num_bikes_available, 
               D1$num_docks_available, 
               col = 'black', pch = 20)
        
        if(K %in% stations$station_id){
          D = stations %>% filter(station_id == K)
          points(D$num_bikes_available, 
                 D$num_docks_available, 
                 col = 'green',
                 pch = 20)
          text(D$num_bikes_available,
               D$num_docks_available, 
               D$station_id,col = 'green')
          
        }
        
        
        plot(stations$num_bikes_available, 
             stations$num_docks_available, 
             pch = 20, 
             col = 'blue',
             main = 'Approximate Curve between for bikes and docks',xlab = 'Number of available bikes for stations', 
             ylab = 'Number of available docks for stations')
        fit = lowess(stations$num_bikes_available~stations$num_docks_available)     
        lines(fit, col = 'red', lty = 'dashed', lwd = 2)
        
    }

        
    if(maptypeInput() == 'Customer'){
      D = stations %>% filter(station_id == K)
          layout(matrix(1:4, 2, 2, byrow = T))
          
          hist(stations$num_bikes_disabled, 
               col = "#75AADB", 
               border = "white",
               xlab = "Number of disable bikes in stations",
               main = "Histogram of disabled bikes")
          
          hist(stations$num_docks_disabled, 
               col = "#75AADB",
               border = "white",
               xlab = "Number of available docks in stations",
               main = "Histogram of disabled docks")
          
          plot(stations$num_bikes_disabled, 
               stations$num_docks_disabled, 
               pch = 20, 
               col = 'blue',
               main = 'Disabled bikes and Disabled docks',
               xlab = 'Number of disabled bikes for stations', 
               ylab = 'Number of aisabled docks for stations')
          
          D0 = stations %>% filter(is_returning == 0)
          
          D1 = stations %>% filter(station_status == 'out_of_service')
          
          points(D0$num_bikes_disabled, 
                 D0$num_docks_disabled, 
                 col = 'red', 
                 pch = 20)
          
          points(D1$num_bikes_disabled, 
                 D1$num_docks_disabled, 
                 col = 'black',
                 pch = 20)
          
          if(K %in% stations$station_id){
            D = stations %>% filter(station_id == K)
            points(D$num_bikes_disabled, 
                   D$num_docks_disabled, 
                   col = 'green', 
                   pch = 20)
            
            text(D$num_bikes_disabled,
                 D$num_docks_disabled, 
                 D$station_id)
          }
          
          fit = lowess(stations$num_docks_disabled~stations$num_bikes_disabled)     
          lines(fit, col = 'red', lty = 'dashed', lwd = 2)
          
          plot(stations$num_bikes_available, 
               stations$num_bikes_disabled, 
               pch = 20, 
               col = 'blue',
               main = 'Available and Disabled stations',
               xlab = 'Number of available bikes for stations', 
               ylab = 'Number of disabled bikes for stations')
          fit = lowess(stations$num_bikes_disabled~stations$num_bikes_available)     
          lines(fit, col = 'red', lty = 'dashed', lwd = 2)
    }
    if(maptypeInput() == 'Model'){
      
    fit = lm(stations$num_bikes_available~ 
               stations$num_docks_available +
               stations$num_bikes_disabled + 
               stations$num_docks_disabled)
    layout(matrix(1:4, 2,2))
    plot(fit)
    }
  })
}

