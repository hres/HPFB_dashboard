ui<-tagList(
    useShinyjs(),
    
    dashboardPage(
        
        dashboardHeader(title='HPFB Performance Metric Dashboard'),
        
        dashboardSidebar(width=200,
                         sidebarMenu(id='sidebar',
                                     
                         menuItem('Branch metrics',tabName='branch'),
                         menuItem('Directorate metrics',tabName='directorate'),
                         
                         conditionalPanel(
                             condition="input.sidebar=='directorate' ",
                
                             selectInput('selectdir',label=div(style="color:white;","Select a directorate"),
                                         choices=c('Food','Medical Devices','NHP','TPD','MHPD','BGTD','VDD'))
                         )
                                     
                         )
        ),
        
        dashboardBody(
            
            shinyDashboardThemes(
                theme = "poor_mans_flatly"
            ),
            
            tabItems(
            tabItem(tabName='branch'),
            tabItem(tabName='directorate',
                    fluidRow(
                        column(12,
                               uiOutput('table_title'),
                               h4('Cost Recovery Performance'),
                               DT::dataTableOutput('table_output')),
                        
                        conditionalPanel(
                            condition="input.selectdir=='TPD' ||
                                       input.selectdir=='BGTD' ||
                                       input.selectdir=='Medical Devices'",
                            
                            column(12,
                                   h4('Non Cost Recovery and Workload Performance'),
                                   DT::dataTableOutput('table_output2'))
                        )
                    ))
            
            )
        )
    )
)