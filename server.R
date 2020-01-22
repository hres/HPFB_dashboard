
#function to transform and standarize tables
clean_table<-function(tb){
  tb[tb=='No data']<-NA
  
  if(class(tb$YTD)!='numeric'){
  tb$YTD<-percent(as.numeric(ds$YTD))
}else{
  tb$YTD<-percent(tb$YTD)
}

return(tb)

}



shinyServer(function(input, output) {

   table_data<-reactive({
  
     if(input$selectdir=='Food') ds<-food%>%clean_table()
     
     if(input$selectdir=='Medical Devices') ds<-med_device%>%clean_table()
     
     if(input$selectdir=='NHP') ds<-nhp%>%clean_table()
     
     if(input$selectdir=='TPD')  ds<-tpd%>%clean_table()
      
     if(input$selectdir=='MHPD') ds<-mhpd%>%clean_table()
     
     if(input$selectdir=='BGTD') ds<-bgtd%>%clean_table()
     
     if(input$selectdir=='VDD') ds<-vet%>%clean_table()
     
   })
   
   output$table_title<-renderUI({
      
      title<-data.frame(title=c('Pre-Market - Food','Pre-Market - Medical Devices','Pre-Market - Natural Health Products',
                                'Pre-Market - Prescription Pharmaceuticals',
                                'Post-Market - Marketed Health Products',
                                'Pre-Market - Biologics',
                                'Pre-Market - Veterinary Drugs'),
                        input=c('Food','Medical Devices','NHP','TPD','MHPD','BGTD','VDD'))
      
      tags$h3(title$title[title$input==input$selectdir])
   })
   
   output$table_output<-renderDataTable({
     
      table<-table_data()
      
      if(input$selectdir=='MHPD'){
         
         DT::datatable(table)%>%formatStyle(
            'Current.month',
            backgroundColor = styleEqual(c(1,2),c('Red','Green'))%>%
               formatStyle(
                  0,
                  target='row',
                  fontWeight=styleEqual(c(1,6,10),c('bold','bold','bold'))
               )
         ) 
         
         
      }else{
         
        DT::datatable(table)%>%formatStyle(
           'Current.month',
           backgroundColor = styleEqual(c(1,2),c('Red','Green'))
        ) 
         
      }
      
     
   })
   
   
   
})
