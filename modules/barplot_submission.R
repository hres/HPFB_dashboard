#module for submission volumnes 


submissionUI<-function(id,title){
  ns<-NS(id)
  
  box(title=title,plotlyOutput(ns('plot1'))%>%withSpinner())
           
}


submissionserver<-function(input,output,session,data){
  
  output$plot1<-renderPlotly({
    
    #add vertical lines indicating time completion 
    #need to consider the delay in reporting, 2 months?
    current_month_percent<-round(current_report_month/12,1)
    
    
    data<-data()%>%filter(!is.na(`%.Previous.months`))
    data<-data%>%arrange(X1)
    
    vline <- function(x = 0, color = "#d3d3d3") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color,dash='dashdot',width=1)
      )
    }
    
    vline_time<-function(x = 0, color = "#8b0000") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color,dash='dashdot',width=1)
      )
    }
    
    
    plot_ly(data,y=~X1,x=~`%.Previous.months`,type='bar',name='Previous months',orientation = 'h')%>%
      add_trace(x=~`%.Current`,name='Current')%>%
      add_trace(x=~`%.Remaining`,name='Remaining')%>%
      layout(xaxis=list(title='Number of submission/Total forecasted submissions 19-20',tickformat='%'),
             yaxis=list(title=''),
             barmode='stack',
             shapes = list(vline(1),vline_time(current_month_percent))
      )%>%
      add_annotations(text=data$Forecast,
                      x=data$`%.Previous.months`+data$`%.Current`+data$`%.Remaining`+0.1,
                      y=c(0:(length(data$Forecast)-1)),
                      showarrow=FALSE)
  })
  
  
}