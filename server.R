
#function to transform and standarize tables
clean_table<-function(tb){
  tb[tb=='No data']<-NA

  if(class(tb$YTD)!='numeric'){
  tb$YTD<-percent(as.numeric(tb$YTD),na.rm=T)
}else{
  tb$YTD<-percent(tb$YTD,na.rm=T)
}

return(tb)

}



shinyServer(function(input, output) {

   table_data<-reactive({
      
     ds<-list(length=2)
  
     if(input$selectdir=='Food') {
        ds[[1]]<-food%>%clean_table()
        ds[[2]]<-NULL
     }
     
     if(input$selectdir=='Medical Devices') {
        ds[[1]]<-med_device%>%clean_table()
        ds[[2]]<-med_device_ncr
     }
     
     if(input$selectdir=='NHP') {
        ds[[1]]<-nhp%>%clean_table()
        ds[[2]]<-NULL
     }
     
     if(input$selectdir=='TPD')  {
        ds[[1]]<-tpd%>%clean_table()
        ds[[2]]<-NULL
     }
      
     if(input$selectdir=='MHPD') {
        ds[[1]]<-mhpd%>%clean_table()
        ds[[2]]<-NULL
     }
     
     if(input$selectdir=='BGTD') {
        ds[[1]]<-bgtd%>%clean_table()
        ds[[2]]<-bgtd_ncr
     }
     
     if(input$selectdir=='VDD') ds[[1]]<-vet%>%clean_table()
     
     return(ds)
     
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
     
      table<-table_data()[[1]]
      
      if(input$selectdir=='MHPD'){
         
         output<-DT::datatable(table,
             options = list(autoWidth=TRUE,
                            columnDefs = list(list(targets = 0, visible = FALSE),
                                              list(targets=1,width='250px'))))%>%
               formatStyle(
                  0,
                  target='row',
                  fontWeight=styleEqual(c(1,6,10),c('bold','bold','bold'))
               )%>%
               formatStyle(
                  'Current.month',
                  backgroundColor = styleEqual(c(1,2),c('Red','Green'))
               )
         
         
         
      }else{
         
        output<-DT::datatable(table,rownames=F)%>%
            formatStyle(
          'Current.month',
           backgroundColor = styleEqual(c(0,2),c('Red','Green'))
        )
           
      }
      
      
      if(any(grepl('load',colnames(table)))){
         
         output%>%
         formatStyle(
            'load',
            backgroundColor = styleEqual(c(1,2),c('Red','Green'))
         )
      }else{
         output
      }
      
      
     
   })
   
   
   
   output$table_output2<-renderDataTable({
      
      table<-table_data()[[2]]
      
      DT::datatable(table,rownames=F)
   })
   
   
   
})
