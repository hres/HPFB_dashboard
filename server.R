
#function to transform and standarize tables
clean_table<-function(tb){
   tb[tb=='No data']<-NA

  if(class(tb$YTD)!='numeric'){
  tb$YTD<-percent(as.numeric(tb$YTD))
}else{
  tb$YTD<-percent(tb$YTD)
}

return(tb)

}



shinyServer(function(input, output,session) {
   
   
   #set global options for datatables:
   options(DT.options = list(pageLength=15, language = list(search = 'Filter:')))
   

   table_data<-reactive({
      
     ds<-list(length=4)
  
     if(input$selectdir=='Food') {
        ds[[1]]<-food%>%clean_table()
        ds[[2]]<-NULL
        ds[[3]]<-food_raw_ncr%>%clean_ds_forplot()
        
     }
     
     if(input$selectdir=='Medical Devices') {
        ds[[1]]<-med_device_ncr
        ds[[2]]<-med_device%>%clean_table()
        ds[[4]]<-md_raw_cr%>%clean_ds_forplot()
        ds[[3]]<-md_raw_ncr%>%clean_ds_forplot()
     }
     
     if(input$selectdir=='NHP') {
        ds[[1]]<-nhp%>%clean_table()
        ds[[2]]<-NULL
        ds[[3]]<-nhp_raw_ncr%>%clean_ds_forplot()
     }
     
     if(input$selectdir=='TPD')  {
        ds[[1]]<-tpd_ncr
        ds[[2]]<-tpd%>%clean_table()
        ds[[4]]<-tpd_raw_cr%>%clean_ds_forplot()
        ds[[3]]<-tpd_raw_ncr%>%clean_ds_forplot()
     }
      
     if(input$selectdir=='MHPD') {
        ds[[1]]<-mhpd%>%clean_table()
        ds[[2]]<-NULL
     }
     
     if(input$selectdir=='BGTD') {
        ds[[1]]<-bgtd_ncr
        ds[[2]]<-bgtd%>%clean_table()
        ds[[4]]<-bgtd_raw_cr%>%clean_ds_forplot()
        ds[[3]]<-NULL
     }
     
     if(input$selectdir=='VDD') {
        ds[[1]]<-vet%>%clean_table()
        ds[[2]]<-NULL
        ds[[3]]<-vdd_raw_ncr%>%clean_ds_forplot()
     }
     
     return(ds)
     
   })
   
   
   titles<-reactive(
      
      data.frame(title=c('Pre-Market - Food','Pre-Market - Medical Devices','Pre-Market - Natural Health Products',
                         'Pre-Market - Prescription Pharmaceuticals',
                         'Post-Market - Marketed Health Products',
                         'Pre-Market - Biologics',
                         'Pre-Market - Veterinary Drugs'),
                 input=c('Food','Medical Devices','NHP','TPD','MHPD','BGTD','VDD'))
   )
   
   output$table_title<-renderUI(
      tags$h3(titles()$title[titles()$input==input$selectdir])
   )
   
   
   output$historic_table_title<-renderUI(
      tags$h3(titles()$title[titles()$input==input$selectdir])
   )
   
   output$table_output<-renderDataTable({
     
      table<-table_data()[[1]]
      
      if(input$selectdir=='MHPD'){
         
         output<-DT::datatable(table,class = 'cell-border stripe',
             options = list(autoWidth=TRUE,
                            columnDefs = list(list(targets = 0, visible = FALSE),
                                              list(targets=1,width='150px'))))%>%
               formatStyle(
                  0,
                  target='row',
                  fontWeight=styleEqual(c(1,6,10),c('bold','bold','bold'))
               )
         
         
      }else{
         
        output<-DT::datatable(table,rownames=F,
                              class = 'cell-border stripe')%>%
            formatStyle(
           c('Work.load'),
           color = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050')),
           backgroundColor = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050'))
          
        )
           
      }
      
      
      if(any(grepl('Current.month',colnames(table)))){
         
         output%>%
         formatStyle(
            'Current.month',
            backgroundColor = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050')),
            color = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050'))
         )
         
      }else if(any(grepl('Performance',colnames(table)))){
         output%>%
            formatStyle(
               'Performance',
               backgroundColor = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050')),
               color = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050'))
            )
      }
      
      
     
   })
   
   
   
   output$table_output2<-renderDataTable({
      
      main_list_ati <- ati$X1[is.na(ati$X2)]
      sub_list_ati<-ati$X1[!is.na(ati$X2)]
      sub_list_ati<-sub_list_ati[!is.na(sub_list_ati)]
      
      ati_cal <- as.numeric(unlist(t(ati[grepl("On time", ati$X2),c(3:ncol(ati))])))/as.numeric(unlist(t(ati[grepl("Total", ati$X2),c(3:ncol(ati))])))
      ati_cal <- as.data.frame(split(ati_cal, 3:ncol(ati)))
      ati_cal$category<-sub_list_ati
      
      index_main_list_ati<-c(which(is.na(ati$X2)),nrow(ati)+1)
      main_list_count_ati<-(diff(index_main_list_ati,lag=1)-3)/2
      main_list_ati<-unlist(mapply(rep,main_list_ati,main_list_count_ati,USE.NAMES =F,simplify=TRUE))
      ati_cal$cat2<- as.list(main_list_ati)
      
      ati_cal <- ati_cal[c("cat2", "category", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")]
      
      DT::datatable(ati_cal%>% mutate_if(is.numeric, ~round(., 2)), class = 'cell-border stripe',
                    options = list(autoWidth=TRUE,columnDefs = list(list(targets = 0, visible = FALSE)),
                                   list(targets=1,width='250px')))  %>%
         
         formatPercentage(c("X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15"))%>%
         
         formatStyle(
            color = styleEqual(c(1,0.94,0.98),c("mediumspringgreen","gold", "gold")),
            c(3:ncol(ati_cal)),
            myJScolor = "value == 1 ? \"mediumspringgreen\"  :isNaN(parseFloat(value)) ? \"lightskyblue\" : value",
            #class(myJScolor) = "JS_EVAL"
            backgroundColor = myJScolor
         )%>%
         formatStyle(
            0,
            target='row',
            #fontWeight=styleEqual(c(1),rep('bold',1))
            fontWeight=styleEqual(c("cat2"),rep('bold',1))
         )
   })
   
   
   output$table_output3<-renderTable({
      
      mhpd_risk
      
   })
   
   output$table_output4<-renderTable({
      
      shiny::validate(
              need(nrow(mhpd_safety)>0,'No Summary Safety Reviews were published'))
      
      mhpd_safety
   })
   
   
   output$time_track_tb<-renderDataTable({
     
     DT::datatable(time_track,rownames=F,class = 'cell-border stripe')%>%
         formatCurrency('Outstanding.$')%>%
         formatPercentage('Compliance')%>%
         formatStyle(
            
         c(2:13),
         backgroundColor = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050')),
         color = styleEqual(c(0,1,2),c('#C00000','#FFC000','#00B050'))
       
       )
   })
   
   
   output$ati_tb<-renderDataTable({
      main_list_ati <- ati$X1[is.na(ati$X2)]
      sub_list_ati<-ati$X1[!is.na(ati$X2)]
      sub_list_ati<-sub_list_ati[!is.na(sub_list_ati)]
      
      ati_cal <- as.numeric(unlist(t(ati[grepl("On time", ati$X2),c(3:ncol(ati))])))/as.numeric(unlist(t(ati[grepl("Total", ati$X2),c(3:ncol(ati))])))
      ati_cal <- as.data.frame(split(ati_cal, 3:ncol(ati)))
      
      ati_cal$category<-sub_list_ati
      index_main_list_ati<-c(which(is.na(ati$X2)),nrow(ati)+1)
      main_list_count_ati<-(diff(index_main_list_ati,lag=1)-3)/2
      main_list_ati<-unlist(mapply(rep,main_list_ati,main_list_count_ati,USE.NAMES =F,simplify=TRUE))
      ati_cal$cat2<- as.list(main_list_ati)
      
      ati_cal <- ati_cal[c("cat2", "category", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")]
      
      DT::datatable(ati_cal%>% mutate_if(is.numeric, ~round(., 2)), class = 'cell-border stripe',
                    options = list(autoWidth=TRUE,columnDefs = list(list(targets = 0, visible = FALSE)),
                                   list(targets=1,width='250px')))  %>%
         
         formatPercentage(c("X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15"))%>%
         formatStyle(
            color = styleEqual(c(1,0.94,0.98),c("mediumspringgreen","gold", "gold")),
            c(3:ncol(ati_cal)),
            myJScolor = "value == 1 ? \"mediumspringgreen\"  :isNaN(parseFloat(value)) ? \"lightskyblue\" : value",
            #class(myJScolor) = "JS_EVAL"
            backgroundColor = myJScolor
            
         )%>%
         formatStyle(
            0,
            target='row',
            #fontWeight=styleEqual(c(1),rep('bold',1))
            fontWeight=styleEqual(c("cat2"),rep('bold',1))
         )
    
   })
   
   
   output$overall_cr<-renderPlotly(
      plot_ly(revenue_tbs[[1]],x=~id,y=~`Collections forecast`,type='bar',name='Collection forecast')%>%
         add_trace(y=~`Collections (cumulative)`,name='Collections (cumulative)')%>%
         add_trace(y=~Billings,name='Billings',type='scatter',mode='lines+markers')%>%
         layout(xaxis=list(title=''),
                yaxis=list(title='Revenue ($)')
         )
   )
   
   
   callModule(barchartserver,'plot_1',reactive(revenue_tbs[[2]]),reactive(revenue_tbs[[3]]))
   callModule(barchartserver,'plot_2',reactive(revenue_tbs[[4]]),reactive(revenue_tbs[[5]]))
   callModule(barchartserver,'plot_3',reactive(revenue_tbs[[6]]),reactive(revenue_tbs[[7]]))
   callModule(barchartserver,'plot_4',reactive(revenue_tbs[[8]]),reactive(revenue_tbs[[9]]))
   
   
   callModule(submissionserver,'submission_1',reactive(pharma_sv))
   callModule(submissionserver,'submission_2',reactive(otc_sv))
   callModule(submissionserver,'submission_3',reactive(bio_sv))
   callModule(submissionserver,'submission_4',reactive(medical_sv))
   
   
   # tab for historical data
   
   heightcontrol<-reactive({

      if(input$selectdir=='MHPD'){

         height=600
      }else{
         height=350
      }

      return(height)
   })
   
   output$historical_table_output_ui<-renderUI({
      plotOutput('historical_table_output',height=heightcontrol())%>%withSpinner()
   })
      

   
   output$historical_table_output<-renderPlot({
      
      color<-c('low'='#C00000','mid'='#FFC000','high'='#00B050')
      
      if(input$selectdir=='MHPD'){
      
      data<-mhpd_cal%>%clean_ds_forplot()
         
      p<-ggplot(data,aes(x=month,y=category,fill=percent_cat,label=percent(percent)))+
            geom_tile(color='grey')+
            scale_fill_manual(values=color,na.value='grey90')+
            theme_minimal(base_size=15)+
            theme(legend.position='none',
                  axis.ticks=element_blank(),
                  axis.text.x=element_text(colour='grey50'))+
            labs(x='',y='')+
            facet_col(vars(cat2),scales='free_y',space='free')
            
         
      }else{
      
      data<-table_data()[[3]]
      
      p<-ggplot(data,aes(x=month,y=category,fill=percent_cat,label=percent(percent)))+
         geom_tile(color='grey')+
         scale_fill_manual(values=color,na.value='grey90')+
         theme_minimal(base_size=15)+
         theme(legend.position='none',
               axis.ticks=element_blank(),
               axis.text.x=element_text(colour='grey50'))+
         labs(x='',y='')
      
      }
      
      p
      
   })
   
   
   output$historical_table_output2<-renderPlot({
      
      data<-table_data()[[4]]
      
      color<-c('low'='#C00000','mid'='#FFC000','high'='#00B050')
      
      p<-ggplot(data,aes(x=month,y=category,fill=percent_cat,label=percent(percent)))+
         geom_tile(color='grey')+
         scale_fill_manual(values=color,na.value='grey90')+
         theme_minimal(base_size=15)+
         theme(legend.position='none',
               axis.ticks=element_blank(),
               axis.text.x=element_text(colour='grey50'))+
         labs(x='',y='')
      
      p
   })
   
})
