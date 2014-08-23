
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)


data_raw<-read.csv("phone_GDP.csv")


predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
        
        # If xrange isn't passed in, determine xrange from the models.
        # Different ways of extracting the x range, depending on model type
        if (is.null(xrange)) {
                if (any(class(model) %in% c("lm", "glm")))
                        xrange <- range(model$model[[xvar]])
                else if (any(class(model) %in% "loess"))
                        xrange <- range(model$x)
        }
        
        newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
        names(newdata) <- xvar
        newdata[[yvar]] <- predict(model, newdata = newdata, ...)
        newdata
}

reg_line_values_TIME<-function(data_IN,year_cut){
        
        data_model<-data_IN[,]
        model_fit<-loess(Subscriptors.per100 ~ year,data=data_model)
        lm_predicted<- predictvals(model_fit, "year", "Subscriptors.per100")
        lm_predicted
        
}

reg_line_values_NETWORK<-function(data_IN){
                
        model_fit<-lm(Subscriptors.per100 ~ years.with.network,data=data_IN)
        lm_predicted<- predictvals(model_fit, "years.with.network", "Subscriptors.per100")
        lm_predicted
        
}


plot_zona_Network<-function(datos_zona,year_cut,zone_str){        
        str_title<-paste("Subscriptors.per100 vs Networks at year ",year_cut,"\n",zone_str)
        pred_line<-reg_line_values_NETWORK(datos_zona)
        ggplot(datos_zona, aes(x=years.with.network, y=Subscriptors.per100, size=GPDperCapita,color=GPDperCapita))+
                geom_point() + ggtitle(str_title)+geom_line(data=pred_line, colour="red", size=.8)
        
}

plot_zona_Time<-function(datos_zona,zone_str){        
        str_title<-paste("Evolution of Subscriptors.per100 by Time - ",zone_str)        
        ggplot(datos_zona, aes(x=year, y=Subscriptors.per100, size=GPDperCapita,color=GPDperCapita))+        
                geom_point()+ ggtitle(str_title)
}

plot_zona_Time_Regresion<-function(datos_zona,zone_str,year_cut){        
        str_title<-paste("Subscriptors.per100 by year \n",zone_str)
        pred_line<-reg_line_values_TIME(datos_zona,year_cut)
        ggplot(datos_zona, aes(x=year, y=Subscriptors.per100, size=GPDperCapita,color=GPDperCapita))+        
                geom_point()+ ggtitle(str_title)+geom_line(data=pred_line, colour="red", size=.8)
}


plot_world_Network<-function(datos_zona,year_cut){
        str_title<-paste("Subscriptors.per100 vs Networks at year ",year_cut," - World")
        pred_line<-reg_line_values_NETWORK(datos_zona)
        ggplot(datos_zona, aes(x=years.with.network, y=Subscriptors.per100, colour=region,size=GPDperCapita)) +
                geom_point()+ ggtitle(str_title)+geom_line(data=pred_line, colour="red", size=.8)
}

plot_world_Time<-function(datos_zona){
        ggplot(datos_zona, aes(x=year, y=Subscriptors.per100, colour=region,size=GPDperCapita)) +
                geom_point()+ ggtitle("Evolution of Subscriptors.per100 by Time - World")
}

plot_world_Time_Regresion<-function(datos_zona,year_cut){
        str_title<-paste("Subscriptors.per100 by year - World")
        pred_line<-reg_line_values_TIME(datos_zona,year_cut)
        ggplot(datos_zona, aes(x=year, y=Subscriptors.per100, colour=region,size=GPDperCapita)) +
                geom_point()+ ggtitle(str_title)+geom_line(data=pred_line, colour="red", size=.8)
}


plot_world_Time<-function(datos_zona){
        ggplot(datos_zona, aes(x=year, y=Subscriptors.per100, colour=region,size=GPDperCapita)) +
                geom_point()+ ggtitle("Evolution of Subscriptors.per100 by Time - World")
}

shinyServer(function(input, output) {


        # REGRESSION ----------------------------------------------------------
        
        
        output$REGRESION_plot <- renderPlot({
                
                if (input$ACTIVE_PAGE=="REGRESION"){
                
                        if(input$REGRESION_region=="World"){
                                datos_zona<-data_raw[data_raw$year>=input$REGRESION_year,]
                                model<-lm(Subscriptors.per100 ~ year*GPDperCapita+years.with.network+region,data=datos_zona)                                  
                        }else{
                                datos_zona<-data_raw[data_raw$region==input$REGRESION_region & data_raw$year>=input$REGRESION_year,]
                                model<-lm(Subscriptors.per100 ~ year+GPDperCapita+years.with.network,data=datos_zona)                  
                        }
                        
                        par(mfrow=c(2,2))        
                        plot(model)                
                }
        })
        
        output$REGRESION_summary <- renderPrint({
                                                
                if (input$ACTIVE_PAGE=="REGRESION"){
                        if(input$REGRESION_region=="World"){
                                datos_zona<-data_raw[data_raw$year>=input$REGRESION_year,]
                                model<-lm(Subscriptors.per100 ~ year+GPDperCapita+years.with.network*region,data=datos_zona)                                  
                        }else{
                                datos_zona<-data_raw[data_raw$region==input$REGRESION_region & data_raw$year>=input$REGRESION_year,]
                                model<-lm(Subscriptors.per100 ~ year+GPDperCapita+years.with.network,data=datos_zona)                  
                        }
                        
                        summary(model)
                }
        })
        

        


        # ZONES ------------------------------------------------------------------------------
        
        output$ZONES_plot <- renderPlot({
                
                #if (input$ACTIVE_PAGE=="ZONES"){
                
                        if( input$ZONES_plotType=='T'){
                                if(input$ZONES_region=="World"){
                                
                                        #plot_world_Time(data_raw)    
                                        plot_world_Time_Regresion(data_raw,input$ZONES_year)
                                }else{
                                        datos_zona<-data_raw[data_raw$region==input$ZONES_region,]
                                        plot_zona_Time_Regresion(datos_zona,input$ZONES_region,input$ZONES_year)    
                                }
                                
                        }else{
                                if(input$ZONES_region=="World"){
                                        
                                        datos_zona<-data_raw[data_raw$year==input$ZONES_year,]
                                        plot_world_Network(datos_zona,input$ZONES_year) 
                
                                }else{
                                        datos_zona<-data_raw[data_raw$region==input$ZONES_region & data_raw$year==input$ZONES_year,] 
                                        plot_zona_Network(datos_zona,input$ZONES_year,input$ZONES_region)        
                
                                }                        
                        }
                #}        
        })
  
        output$ZONES_summary <- renderPrint({
        
                #if (input$ACTIVE_PAGE=="ZONES"){
                
                        if( input$ZONES_plotType=='T'){
                                if(input$ZONES_region=="World"){
                                        
                                        datos_zona<-data_raw[data_raw$year=="2012",c(-1,-2,-3,-4)]  
                                }else{
                                        datos_zona<-data_raw[data_raw$year=="2012" & data_raw$region==input$ZONES_region,c(-1,-2,-3,-4)]
                                           
                                }
                                
                        }else{
                                if(input$ZONES_region=="World"){
                                        
                                        datos_zona<-data_raw[data_raw$year==input$ZONES_year,c(-1,-2,-3,-4)]
                                       
                                        
                                }else{
                                        datos_zona<-data_raw[data_raw$region==input$ZONES_region & data_raw$year==input$ZONES_year,c(-1,-2,-3,-4)] 
                                               
                                        
                                }                        
                        }
                        
                        
                        summary(datos_zona)
                #}
        
        })
  
        output$ZONES_table <- renderDataTable({
        
                #if (input$ACTIVE_PAGE=="ZONES"){                
                
                        if(input$ZONES_region=="World"){
                                # CASE WORLD
                                datos_zona<-data_raw[data_raw$year==input$ZONES_year,]
                                data <-datos_zona[,]      
                        }else{
                                # CASE ZONE
                                datos_zona<-data_raw[data_raw$region==input$ZONES_region & data_raw$year==input$ZONES_year,-4]
                                data <-datos_zona[,]      
                        }
                        
                #}
                
        })
  

})
