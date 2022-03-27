# setwd("C:\\Users\\xiaohuanza\\Desktop")
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(tidyr)
library(readxl)


df <- readxl::read_xlsx("shinydata.xlsx") %>%
  select(-c(2,7)) %>%
  separate("STR position", sep = ":", into = c("Chromosome", "Position")) %>%
  mutate(Position = as.integer(Position)) %>%
  mutate(filename = paste0(GroupSource, "_", Chromosome, "_", Position, ".txt")) %>%
  mutate(Genotype = paste0('<a href="./', GroupSource, "/", filename, '" download="', filename, '">', filename, '</a>')) %>%
  select(-filename)




# Garbage Collection
gc()

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                titlePanel('STR datatable'),
                
                
                # hide the red text error
                tags$style(type="text/css",
                           ".shiny-output-error{visibility:hidden;}",
                           ".shiny-output-error:before{visibility:hidden;}"
                ),
                
                # A CSS for every .btn button 
                tags$style(HTML(" 
                    .btn { 
                     color: #2ecc71; 
                     border: 2px #2ecc71 solid; 
                    } 
                    .btn:hover { 
                     color: blue; 
                     background-color: #2ecc71; 
                    } 
                    .btn-default.active, .btn-default:active { 
                     color: #fff; 
                     background-color: #2ecc71; 
                     border-color: #2ecc71; 
                    } 
                   ")), 
                              
                fluidRow(
                  
                  column(2, selectInput('GroupSource', 'Group Source', c(unique(df$GroupSource)))),
                  column(2, uiOutput("chromosomeUI")),
                  column(2, uiOutput("startUI")),
                  column(2, uiOutput("endUI")),
                  column(2, actionButton("search", "Search", width="100px", icon = icon("search")))

                ),
                
                fluidRow(
                  column(12, DT::dataTableOutput('table')),
                  column(12, downloadButton(outputId = "download_data", label = "Download Data"))
                )
)


server <- function(input, output){
  
  
  mydataSoruce <- reactive({
      df %>% filter(GroupSource == input$GroupSource)
  })
  

  output$chromosomeUI <- renderUI({
    
    selectInput('Chromosome',  'Chromosome', c('All',unique(mydataSoruce()$Chromosome)))
    
  })  

  
  mydata <- reactive({
    
    if (input$Chromosome != "All") {
        mydataSoruce() %>% filter(Chromosome == input$Chromosome)
    } else {
        mydataSoruce()
    }
    
  })
  
  
  output$startUI <- renderUI({
    
    numericInput("start", "Start input", value= min(mydata()$Position))
    
  })
  
  output$endUI <- renderUI({
    
    numericInput("end", "End input", value= max(mydata()$Position))
    
  })
  
  
  
  search_result <- eventReactive(input$search, {
    
    mydata() %>% filter(Position >= input$start & Position <= input$end)
    
  })
  
  
  
  output$table <- renderDT(DT::datatable({
    search_result()
  },  escape = FALSE,   selection = "single"))
  
  
  
  output$download_data <- downloadHandler(
      filename = "Data.csv",
      content = function(file){
        write.csv( search_result() %>%  mutate(Genotype = paste0(GroupSource, "_", Chromosome, "_", Position, ".txt")),
                  file)
      }
    )

  
}



shinyApp(ui = ui, server = server)
