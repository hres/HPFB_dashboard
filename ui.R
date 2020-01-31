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
                         ),
                         
                         menuItem('Historical Comparison',tabName='history'),
                         menuItem('PIPs',tabName='pips')
                                     
                         )
        ),
        
        dashboardBody(
            
            shinyDashboardThemes(
                theme = "poor_mans_flatly"
            ),
            
            tabItems(
            tabItem(tabName='branch',
                    
                    tabsetPanel(
                        tabPanel(title='Time Tracking',value='time_track',
                                 fluidRow(
                                     column(12,dataTableOutput('time_track_tb'))
                                 )),
                        
                        tabPanel(title='Access to Information',
                                 fluidRow(
                                     column(12,dataTableOutput('ati_tb'))
                                 )),
                        
                        tabPanel(title='Revenue',
                                 fluidRow(box(title='Overall Cost Recovery',width=12,status='primary',solidHeader = T,
                                              plotlyOutput('overall_cr')%>%withSpinner())),
                                 
                                  barchartUI('plot_1','Pharmaceutical Drug Evaluation','Biologic Drug Evaluation'),
                                  barchartUI('plot_2','Medical Device Evaluation','Veterinary Drug Evaluation'),
                                  barchartUI('plot_3','Drug Right to Sell','Medical Device Right to Sell'),
                                  barchartUI('plot_4','Drug Master File','Certificate of Supplementary Protection')
                                 
                                 ),
                        
                        tabPanel(title='Volume Indicators',
                                 
                            fluidRow(
                                 submissionUI('submission_1','Rx Pharma'),
                                 submissionUI('submission_2','OTC Pharma')),
                            fluidRow(
                                 submissionUI('submission_3','Biologics'),
                                 submissionUI('submission_4','Medical Devices'))
                        )
                    )
                    
                    ),
            
            tabItem(tabName='directorate',
                    fluidRow(
                        column(12,
                               uiOutput('table_title'),
                               box(title='Cost Recovery Performance',width=12,solidHeader=T,status='primary',
                                   DT::dataTableOutput('table_output')
                               )
                              ),
                        
                        conditionalPanel(
                            condition="input.selectdir=='BGTD' ||
                                       input.selectdir=='Medical Devices'",
                            
                            column(12,
                                   br(),
                                   br(),
                                   box(title='Non Cost Recovery and Workload Performance',width=12,solidHeader=T,status='primary',
                                       DT::dataTableOutput('table_output2'))
                        ))
                    ))
            
            )
        )
    )
)