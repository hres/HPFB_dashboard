ui<-tagList(
    useShinyjs(),
    
    dashboardPage(
        
        dashboardHeader(title='HPFB Performance Metric Dashboard',titleWidth = '500px'),
        
        dashboardSidebar(width=200,
                         sidebarMenu(id='sidebar',
                                     
                         menuItem('Branch metrics',tabName='branch'),
                         menuItem('Directorate metrics',tabName='directorate'),
                         
                         menuItem('Historical Comparison',tabName='history'),
                         
                         conditionalPanel(
                             condition="input.sidebar=='directorate' | input.sidebar=='history' ",
                             
                             selectInput('selectdir',label=div(style="color:white;","Select a directorate"),
                                         choices=c('Food','Medical Devices','NHP','TPD','MHPD','BGTD','VDD'))
                         ),
                         
                         menuItem('PIPs',tabName='pips')
                         )
        ),
        
        dashboardBody(
            
            shinyDashboardThemes(
                theme = "poor_mans_flatly"
            ),
            
            
            tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: Arial, Helvetica, sans-serif;
        font-weight: bold;
        font-size: 20px;
      }'))),
            
            tabItems(
            tabItem(tabName='branch',
                    
                    tabsetPanel(
                        tabPanel(title='Time Tracking',value='time_track',
                                 fluidRow(
                                     column(12,
                                            dataTableOutput('time_track_tb'),
                                            HTML('<b>Metrics:</b> 95%+ green, 85-95% yellow, <85% red' ))
                                 )),
                        
                        tabPanel(title='Access to Information',
                                 fluidRow(
                                     column(12,dataTableOutput('ati_tb'),
                                            HTML('<b>Metrics:</b> 95%+ green, 85-95% yellow, <85% red' ))
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
                               box(title='Non Cost Recovery Performance',width=12,solidHeader=T,status='primary',
                                   DT::dataTableOutput('table_output'),
                                   tags$b('Measurement criteria:'),
                                   br(),
                                   HTML('Performance within target: 90%+ green, 80-89% yellow, <80% red <br>
                                         Workload: 0-10% in backlog green, 11-20% yellow, >20% red')
                               )
                              ),
                        
                        conditionalPanel(
                            condition="input.selectdir=='BGTD' |
                                       input.selectdir=='Medical Devices'|
                                       input.selectdir=='TPD'",
                            
                            column(12,
                                   br(),
                                   br(),
                                   box(title='Cost Recovery Performance',width=12,solidHeader=T,status='primary',
                                       DT::dataTableOutput('table_output2')
                                       )
                        )),
                        
                        conditionalPanel(
                          condition="input.selectdir=='MHPD'",
                          
                          column(12,
                                 br(),
                                 br(),
                                 box(title='Risk Outcomes / Summary Safety Review',width=12,solidHeader=T,status='primary',
                                     tableOutput('table_output3'),
                                     tableOutput('table_output4'))
                          ))
                        
                    )),
            
            tabItem(tabName='history',
                    fluidRow(  
                          column(12,
                               uiOutput('historic_table_title'),
                               box(title='Historical Non Cost Recovery Performance',width=12,solidHeader=T,status='primary',
                                   plotOutput('historical_table_output')%>%withSpinner()
                               )
                        ),
                        conditionalPanel(
                            condition="input.selectdir=='Medical Devices' |
                                       input.selectdir=='TPD'",
                            
                            column(12,
                                   br(),
                                   br(),
                                   box(title='Historical Cost Recovery Performance',width=12,solidHeader=T,status='primary',
                                       plotOutput('historical_table_output2')%>%withSpinner()
                                       )
                            )) 
                    ))
            
            )
        )
    )
)