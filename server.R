library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    observeEvent(input$do,{
        mar3 <<- read.csv(input$of$datapath)
        mar1 <<- read.csv(input$mwpm$datapath)
        
        if(!is.null(mar3) && !is.null(mar1)){
            sp <- str_split_fixed(mar3$Work.Order, "-CWO", 2)
            sp <- as.data.frame(sp)
            names(sp) <- c("MASTER_WO","CHILD_WO")
            mar4 <- cbind(mar3, sp)
            
            mar4$Closed.Date <- as.Date(mar4$Closed.Date, format = "%Y-%m-%d")
            mar4$Year <- year(mar4$Closed.Date)
            mar4$Week <- week(mar4$Closed.Date)
            mar4$Available <- as.numeric(mar4$Available)
            mar4$Successful <- as.numeric(mar4$Successful)
            mar4$CHILD_WO <- as.numeric(mar4$CHILD_WO)
            for(i in 1:nrow(mar4)){
                if (!is.na(mar4$Successful[[i]]) && mar4$Successful[[i]] !=0 ){
                    if((mar4$Successful[[i]]/mar4$Available[[i]]) > 0.95)
                        mar4$SLO.MET[[i]] <- "MET"
                    else
                        mar4$SLO.MET[[i]] <- "NO MET"
                }
                
            }
            
            mar4.1 <- select(mar4, CHILD_WO, SLO.MET, Year, Week)
            colnames(mar4.1)[2] <- "SLO_MET"
            # Joing both dataset
            mar5 <- right_join(mar4.1, mar1, by = "CHILD_WO")
            mar5 <- mar5[,-c(32:33)]
            mar5.1 <<- na.omit(mar5)
            mar5.1 <<- select(mar5.1, Year.x, ASSIGNEE, SLO_MET, ACCOUNT, 
                              TASK_NAME, MASTER_WO, CHILD_WO, Week.x)
            mar5.1 <- unique(mar5.1)
            values <<- reactiveValues(df = mar5.1)
            
            observe({
                values$df <<- subset(mar5.1, ASSIGNEE == input$inSelect &
                                         Year.x == input$inYear & 
                                         SLO_MET == input$inMet &
                                         TASK_NAME == input$inTask)
                
            })
            
            #summaryTable <- cast(mar5.1, ASSIGNEE + SLO_MET + TASK_NAME ~ Year.x)
            
            output$mmsTable <- renderDataTable({
                d <- event_data("plotly_click")
                if (is.null(d)) 
                    #select(mar5.1, Year.x, ASSIGNEE, SLO_MET, ACCOUNT, TASK_NAME, MASTER_WO, CHILD_WO)
                    (select(values$df, Year.x, Week.x,ASSIGNEE, SLO_MET, ACCOUNT, 
                            TASK_NAME, MASTER_WO, CHILD_WO))
                else
                    (select(subset(values$df, Week.x == d$x), Year.x, Week.x,ASSIGNEE, SLO_MET, ACCOUNT, 
                            TASK_NAME, MASTER_WO, CHILD_WO))
            })
            
            output$summaryTable <- renderDataTable({
                cast(data = values$df, ASSIGNEE + SLO_MET + TASK_NAME ~ Year.x)
            })
            
            updateSelectInput(session, "inSelect",
                              label = paste("Select Assignee", 
                                            length(unique(values$ASSIGNEE))),
                              choices = sort(unique(mar5.1$ASSIGNEE)))
            
            updateRadioButtons(session, "inYear",
                               label = paste("Select Year", 
                                             length(unique(values$df$Year.x))),
                               choices = unique(mar5.1$Year.x))
            
            updateRadioButtons(session, "inMet",
                               label = paste("Select Met", 
                                             length(unique(values$SLO_MET))),
                               choices = unique(mar5.1$SLO_MET))
            
            updateSelectInput(session, "inTask",
                              label = paste("Select Task", 
                                            length(unique(values$TASK_NAME))),
                              choices = sort(unique(mar5.1$TASK_NAME)))
            
            output$graphTotal <- renderPlotly({
                tp <- select(values$df, ASSIGNEE, SLO_MET, Week.x, TASK_NAME, Year.x)
                tp <- (count(tp, c('Year.x','ASSIGNEE','Week.x', 'SLO_MET', 'TASK_NAME')))
                
                plot_ly(data = tp,
                        x = ~Week.x,
                        y = ~freq,
                        color = ~SLO_MET,
                        type = "scatter",
                        line = list(shape = 'spline')
                ) %>%
                    layout(title = "Trend per Week per Assignee")
            })
        }
        else
            return(NULL)
        # Close the R session when Chrome closes
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    })
  
})
