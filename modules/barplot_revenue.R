#module for bar charts:

barchartUI<-function(id,title1,title2){
  ns<-NS(id)
  
  fluidRow(box(title=title1,plotlyOutput(ns('plot1'))%>%withSpinner()),
           box(title=title2,plotlyOutput(ns('plot2'))%>%withSpinner()))
}


barchartserver<-function(input,output,session,data1,data2){
  
  output$plot1<-renderPlotly(
    plot_ly(data1(),x=~id,y=~`Collections forecast`,type='bar',name='Collection forecast')%>%
      add_trace(y=~`Collections (cumulative)`,name='Collections (cumulative)')%>%
      add_trace(y=~Billings,name='Billings',type='scatter',mode='lines+markers')%>%
      layout(xaxis=list(title=''),
             yaxis=list(title='Revenue ($)')
            )
  )
  
  
  output$plot2<-renderPlotly(
    plot_ly(data2(),x=~id,y=~`Collections forecast`,type='bar',name='Collection forecast')%>%
      add_trace(y=~`Collections (cumulative)`,name='Collections (cumulative)')%>%
      add_trace(y=~Billings,name='Billings',type='scatter',mode='lines+markers')%>%
      layout(xaxis=list(title=''),
             yaxis=list(title='Revenue ($)')
             )
  )
  
}


