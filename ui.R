
library(shiny)
library(ggplot2)

#data("AdultUCI")
#data<-AdultUCI

#for(i in c(1,3,5,11,12,13)) {data[i] <- lapply(data[i], as.numeric)}

fluidPage(
  
  titlePanel("Classification Korupsi court decision", tags$head()),
  # Copy the line below to make a text input box
  
  
  mainPanel(
    tabsetPanel(id = 'mytab',
                
                tabPanel('Recipe', value = 'datatable', dataTableOutput("dataasli")),
                tabPanel('Histogram', value = 'graph',plotOutput('histoplot')),
                tabPanel('Wordcloud', value = 'wordcloud',plotOutput('wordcloudplot'))
                         
    )
  )
)