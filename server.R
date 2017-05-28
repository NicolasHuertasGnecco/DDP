#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        #Render the message
        output$hola<-renderText({
                paste("Hi",paste(input$name,",",sep=""),"Welcome to the data handler")
                
        })
       
        #Renders the table for x and y axis values
        
        output$tabla<-renderDataTable({
                       if(input$variable=="mtcars"){
                               mtcars[,c(input$Choice1,input$Choice2)]
                       }
                else if(input$variable=="longley"){
                        longley[,c(input$Choice1,input$Choice2)]
                }
               
                else if(input$variable=="USArrests"){
                        USArrests[,c(input$Choice1,input$Choice2)]
                }
                else if(input$variable=="women"){
                        women[,c(input$Choice1,input$Choice2)]
                }
           
                else if(input$variable=="USJudgeRatings"){
                        USJudgeRatings[,c(input$Choice1,input$Choice2)]
                }
                else if(input$variable=="sleep"){
                        sleep[,c(input$Choice1,input$Choice2)]
                }
              
                
                
                
               
                })
        #Generates the selection for x axis values
        output$choice1<-renderUI({
                if(input$variable=="mtcars"){
                        radioButtons("Choice1", "Choose the varible in the X axis ",
                                choices = names(mtcars))
                        
                }
                else if(input$variable=="longley"){
                        radioButtons("Choice1","Choose the varible in the X axis ",
                                choices = names(longley))
                }
          
                else if(input$variable=="USArrests"){
                        
                        radioButtons("Choice1", "Choose the varible in the X axis ",
                                choices = names(USArrests))
                }
                else if(input$variable=="women"){
                        radioButtons("Choice1", "Choose the varible in the X axis ",
                                choices = names(women))
                }
              
                else if(input$variable=="USJudgeRatings"){
                        radioButtons("Choice1", "Choose the varible in the X axis ",
                                choices = names(USJudgeRatings)) 
                }
                else if(input$variable=="sleep"){
                        radioButtons("Choice1", "Choose the varible in the X axis ",
                                choices = names(sleep))
                }
            
                
        })
        
        output$dataf<-renderText({
                paste("You selected",input$variable,"to make your analysis here tha data:")
                
        })
        #Generates the plot with the selected values
        output$plot<-renderPlot({
                tryCatch({
                if(input$Choice1!=""&input$Choice2!="")
                {
                
                if(input$variable=="mtcars"){
                        g<-ggplot(mtcars,aes(x= mtcars[,input$Choice1],y= mtcars[,input$Choice2])) +
                                geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                labs(x=input$Choice1,y=input$Choice2) 
                        
                }
                else if(input$variable=="longley"){
                         g<-ggplot(longley,aes(x= longley[,input$Choice1],y= longley[,input$Choice2])) +
                                 geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                 labs(x=input$Choice1,y=input$Choice2) 
                         
                }
              
                else if(input$variable=="USArrests"){
                        
                        g<-ggplot( USArrests,aes(x= USArrests[,input$Choice1],y= USArrests[,input$Choice2])) +
                                geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                labs(x=input$Choice1,y=input$Choice2) 
                        
                        
                }
                else if(input$variable=="women"){
                        women
                        g<-ggplot( women,aes(x= women[,input$Choice1],y= women[,input$Choice2])) +
                                geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                labs(x=input$Choice1,y=input$Choice2) 
                        
                        
                }
                
                else if(input$variable=="USJudgeRatings"){
                       
                        g<-ggplot( USJudgeRatings,aes(x= USJudgeRatings[,input$Choice1],y= USJudgeRatings[,input$Choice2])) +
                                geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                labs(x=input$Choice1,y=input$Choice2) 
                        
                }
                else if(input$variable=="sleep"){
                        g<-ggplot( sleep,aes(x= sleep[,input$Choice1],y= sleep[,input$Choice2])) +
                                geom_point(size=2)+geom_smooth(method="lm")+ggtitle(paste(input$Choice1,"VS",input$Choice2)) +
                                labs(x=input$Choice1,y=input$Choice2) 
                        
                        
                }
                        g
                }
                else{
                        g<-ggplot()+geom_blank()+annotate("text",label=paste("CHOOSE X and Y"),x=4,y=4,color="red")
                        g
                }
                },
                        error=function(e) {
                                g<-ggplot()+geom_blank()+annotate("text",label=paste("CHOOSE X and Y"),x=4,y=4,color="red")
                               g
                        }
                )
                        
                
          
                
                
        })
        #generates the summary statistiscs for x and y values
        
        output$summary <- renderPrint({
              
      
                if(input$variable=="mtcars"){
                        summary(mtcars[,c(input$Choice1,input$Choice2)])
                }
                else if(input$variable=="longley"){
                        summary(longley[,c(input$Choice1,input$Choice2)])
                }
              
                else if(input$variable=="USArrests"){
                        summary(USArrests[,c(input$Choice1,input$Choice2)])
                }
                else if(input$variable=="women"){
                        
                        summary(women[,c(input$Choice1,input$Choice2)])
                }
                
                else if(input$variable=="USJudgeRatings"){
                        
                        summary(USJudgeRatings[,c(input$Choice1,input$Choice2)])
                }
                else if(input$variable=="sleep"){
                        summary(sleep[,c(input$Choice1,input$Choice2)]) 
                }
                
                
                
                
                
        })
        #generates the selection for y axis values
        output$choice2<-renderUI({
                if(input$variable=="mtcars"){
                        radioButtons("Choice2", "Choose the varible in the Y axis ",
                                choices = names(mtcars))
                        
                }
                else if(input$variable=="longley"){
                        radioButtons("Choice2", "Choose the varible in the Y axis ",
                                choices = names(longley))
                }
         
                else if(input$variable=="USArrests"){
                        
                        radioButtons("Choice2", "Choose the varible in the Y axis ",
                                choices = names(USArrests))
                }
                else if(input$variable=="women"){
                        radioButtons("Choice2", "Choose the varible in the Y axis ",
                                choices = names(women))
                }
                
                else if(input$variable=="USJudgeRatings"){
                        radioButtons("Choice2","Choose the varible in the Y axis ",
                                choices = names(USJudgeRatings)) 
                }
                else if(input$variable=="sleep"){
                        radioButtons("Choice2", "Choose the varible in the Y axis ",
                                choices = names(sleep))
                }
                
                
        })
  
})
