#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(plotly)
library(dplyr) 
library(tidyr) 
library(stringr)
library(readr)

### scraper function (move to separate module!!)
ScrapeQuery <- function(qyear,qkeyword){
  
  # build url
  url <- paste0('https://scholar.google.com/scholar?hl=en&as_ylo=',qyear,'&q=', qkeyword) # Change database option to variable
  
  # sanitize url
  url <- URLencode(url)
  
  # get results
  res <- read_html(url) %>%           # get url
    html_nodes('div.gs_ri h3 a') %>%   # select titles by css selector 
    html_text()                       # extract text
  
  # return results
  return(res)
  #titles: gs_ri h3 a
}

NumResQuery <- function(qyear,qkeyword){
  
  # build url
  url <- paste0('https://scholar.google.com/scholar?hl=en&as_ylo=',qyear,'&q=', qkeyword) # Change database option to variable
  
  # sanitize url
  url <- URLencode(url)
  
  # get results
  res <- read_html(url) %>%           # get url
    html_nodes('div.gs_ab_mdw') %>%   # select number of results by css selector 
    html_text()                       # extract text

  # return results
  return(res)
  #titles: gs_ri h3 a
}
###

ui <- navbarPage(theme = 'mainpage.css',"Living evidence map",

    # Application title
    tabPanel("Map",
             
             # Scpus search (DB ID)
             fluidRow(
                 column(4,
                        textInput('QueryVal', "Enter query:", value = "", width = NULL, placeholder = NULL)
                 ),
                 column(4,
                        textInput('DateVal', "Results since:", value = "", width = NULL, placeholder = NULL)
                        
                        )
                 #column(4,
                       
                 )
             ),
             
             
             # populated using keyword search
             tabsetPanel(
                 tabPanel("Summary",
                          br(),
                          
                          fluidRow(
                              
                              column(8,
                                     h4("Results Found:"),
                                     textOutput("results"),
                                     h3("Summary Information"),
                                     h5("Based on Keyword"),
                                     tableOutput("sumTable"),
                                     plotlyOutput("heatmapPlot")
                                     
                                     
                                     ),
                                     
                                     
                                     
                             
                              )
                          )
                          
                 )

    # Sidebar?
    
    #sidebarLayout(
     #   sidebarPanel(

     #   ),


      #  mainPanel(

       # )
    #)
)


# Define server logic 
server <- function(input, output) {
  
    testscrape<- eventReactive(input$QueryVal,{
      data.frame(ScrapeQuery('2016',input$QueryVal))}) # replace 2016 w DateVal
    resnum<- eventReactive(input$QueryVal,{
      if (input$QueryVal == "")
        return(0)
      isolate({
      res<-data.frame(NumResQuery('2016',input$QueryVal))
      res2<-str_extract(res, "About\\s*(.*?)\\s*results") 
      res3<-gsub(".*About (.+) results.*", "\\1", res2)
      res4<-as.numeric(parse_number(res3))
      })
      })
    
    output$results<- eventReactive(input$QueryVal,{
      if (input$QueryVal == "")
        return(0)
      isolate({
        res<-data.frame(NumResQuery('2016',input$QueryVal))
        res2<-str_extract(res, "About\\s*(.*?)\\s*results") 
        res3<-gsub(".*About (.+) results.*", "\\1", res2)
        res4<-as.numeric(parse_number(res3))
      })
    })
    # replace 2016 w DateVal
    
  ## main table
    output$sumTable <- renderTable({
      if (input$QueryVal == "")
        return(0)
      isolate({ScrapeQuery('2016',input$QueryVal)}) # replace 2016 w DateVal
      
    },width = '80%', rownames = TRUE, colnames = FALSE)
    
    output$heatmapPlot <- renderPlotly({
      if (input$QueryVal == "")
        return(0)
      isolate({
        plot_ly(x = input$QueryVal, y = "2000", z = resnum(), type = "heatmap") # caps NROW req.for map is testscrape row number used for z
      })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
