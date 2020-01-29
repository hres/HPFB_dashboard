#module for submission volumnes 


submissionUI<-function(id,title){
  ns<-NS(id)
  
  box(title=title,plotlyOutput(ns('plot1'))%>%withSpinner())
           
}


submissionserver<-function(input,output,session,data){
  
  output$plot1<-renderPlotly({
    
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
    
    
    plot_ly(data(),y=~X1,x=~`%.Previous.months`,type='bar',name='Previous months',orientation = 'h')%>%
      add_trace(x=~`%.Current`,name='Current')%>%
      add_trace(x=~`%.Remaining`,name='Remaining')%>%
      layout(xaxis=list(title='Number of submission/Total forecasted submissions 19-20',tickformat='%'),
             yaxis=list(title=''),
             barmode='stack',
             shapes = list(vline(1))
      )
  })
  
  
}