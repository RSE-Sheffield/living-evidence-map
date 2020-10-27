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

### scraper functions (move to separate module!!)
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
                                     textOutput("plotdf"),
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
  
    ## data frames from scrapers
    testscrape<- eventReactive(input$QueryVal,{
      data.frame(ScrapeQuery(input$DateVal,input$QueryVal))}) # replace ateVal
    resnum<- eventReactive(input$QueryVal,{
      if (input$QueryVal == "")
        return(0)
      isolate({
      res<-data.frame(NumResQuery(input$DateVal,input$QueryVal))
      res2<-str_extract(res, "About\\s*(.*?)\\s*results") 
      res3<-gsub(".*About (.+) results.*", "\\1", res2)
      res4<-as.numeric(parse_number(res3))
      })
      })
    
    ## static table dates for testing - should be changed to user input variables?

    daterange<- ({
      c(2015,2016,2017,2018,2019,2020)
    })
    #print(daterange[5])
    
    # needs cleaning up
    output$results<- eventReactive(input$QueryVal,{
      if (input$QueryVal == "")
        return(0)
      isolate({
        res<-data.frame(NumResQuery(input$DateVal,input$QueryVal))
        res2<-str_extract(res, "About\\s*(.*?)\\s*results") 
        res3<-gsub(".*About (.+) results.*", "\\1", res2)
        res4<-as.numeric(parse_number(res3))
      })
    })
    
    
  ## main table of titles
    output$sumTable <- renderTable({
      if (input$QueryVal == "")
        return(0)
      isolate({ScrapeQuery(input$DateVal,input$QueryVal)}) # replace DateVal
      
    },width = '80%', rownames = TRUE, colnames = FALSE)
    

    output$plotdf<- renderText({
      if (input$QueryVal == "")
        return(0)
      isolate({
          res<-data.frame(NumResQuery(input$DateVal,"grinding"))
          res2<-str_extract(res, "About\\s*(.*?)\\s*results") 
          res3<-gsub(".*About (.+) results.*", "\\1", res2)
          res4<-as.numeric(parse_number(res3))
          print(res4)
        })
        
      
    })
    
    # plot formatting to be fixed - bubble chart in table format better?
    output$heatmapPlot <- renderPlotly({
      if (input$QueryVal == "")
        return(0)
      isolate({
        plot_ly(x = input$QueryVal, y = daterange, z = resnum(), type = "heatmap") # caps NROW req.for map is testscrape row number used for z
      })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
