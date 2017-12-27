pckg = c("shiny","dplyr","readxl","readr","tidyr","stringr","lubridate",
         "reshape", "plotly", "plyr")

is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
} 

for(i in 1:length(pckg)) {
    if (!is.installed(pckg[i])){
        install.packages(pckg[i])
    }
}

library(shiny)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(reshape)
library(plotly)
library(plyr)

options(shiny.maxRequestSize = 9*1024^2)
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Met per Assignee"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("of", "Please select your OF File", accept = ".csv"),
            fileInput("mwpm", "Please select your MWPM Task Template File", 
                      accept = ".csv"),
            radioButtons("inYear", label = h3("Select Year"), ""),
            radioButtons("inMet", label = h3("Select MET"), ""),
            selectInput("inSelect", label = h3("Select Name"), ""),
            selectInput("inTask", label = h3("Select Task"), ""),
            actionButton("do", "Upload Files"),
            plotlyOutput("graphTotal")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("summaryTable"),
            dataTableOutput("mmsTable")
        )
    ))
)
