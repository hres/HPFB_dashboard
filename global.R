library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(openxlsx)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(scales)
library(lubridate)
library(purrr)
library(DT)
library(tibble)

#set parameter for modules/barplot_submission
current_report_month<-12


source('modules/barplot_revenue.R')
source('modules/barplot_submission.R')

#load data:
options(digits=3)

#transpose data frame
reformat_table<-function(ds){
  ds<-ds%>%
    `row.names<-`(.[, 1]) %>% 
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>% 
    .[-1,] 
  
  ds[,]<-lapply(ds[,],as.numeric) 
  ds$id<-rownames(ds)
  ds$id<-factor(ds$id,levels=paste0('P',c(1:12)))
  return(ds)
  
}

#load data for performance tables
bgtd<-read.xlsx('_For testing/BGTD/Operational Dashboard - biologics data.xlsx',sheet=1,rows=c(3:11),colNames=T)
bgtd_ncr<-read.xlsx('_For testing/BGTD/Operational Dashboard - biologics data.xlsx',sheet=2,rows=c(3:14),colNames=T)

food<-read.xlsx('_For testing/FOOD/Operational Dashboard - food data.xlsx',sheet=1,rows=c(3:8),colNames=T)

nhp<-read.xlsx('_For testing/NNHPD/Operational Dashboard - NHP data.xlsx',sheet=1,rows=c(3:9),colNames=T)

med_device<-read.xlsx('_For testing/TPD/Operational Dashboard - med device data.xlsx',sheet=1,rows=c(3:14),colNames=T,fillMergedCells = TRUE)
med_device_ncr<-read.xlsx('_For testing/TPD/Operational Dashboard - med device data.xlsx',sheet=2,rows=c(3:12),colNames=T,fillMergedCells = TRUE)

tpd<-read.xlsx('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',sheet=1,rows=c(3:11),colNames=T)
tpd_ncr<-read.xlsx('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',sheet=2,rows=c(3:15),colNames=T)

mhpd<-read.xlsx('_For testing/MHPD/Operational Dashboard - post-market data.xlsx',sheet=1,rows=c(3:16),colNames = T)
mhpd_risk<-read.xlsx('_For testing/MHPD/Operational Dashboard - post-market data.xlsx',sheet='Risk outcomes',rows=c(1:7),colNames = T)
mhpd_safety<-read.xlsx('_For testing/MHPD/Operational Dashboard - post-market data.xlsx',sheet='Risk outcomes',startRow=10,colNames = T)

vet<-read.xlsx('_For testing/VDD/Operational Dashboard - vet drugs data.xlsx',sheet=1,rows=c(3:11),colNames = T)
time_track<-read.xlsx('_For testing/Branch Tables - DO NOT EDIT/HPFB Time Tracking for Operational Dashboard.xlsx',sheet=1,rows=c(3:13),colNames = T)

#------------------------------------
#load data for revenue plots
revenue_tbs<-list()
for (i in 1:9){
  revenue_tbs[[i]]<-read.xlsx('_For testing/BSFO/Operational Dashboard - revenue data.xlsx',sheet=i,rows=c(1:4),cols=c(1:13),colNames = T)
}

revenue_tbs<-lapply(revenue_tbs,reformat_table)

#------------------------------------
#load data for submission volume

pharma_sv<-read.xlsx('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',sheet=3,rows=c(153:161),cols=c(1:9),colNames=T)
bio_sv<-read.xlsx('_For testing/BGTD/Operational Dashboard - biologics data.xlsx',sheet=3,rows=c(66:74),cols=c(1:9),colNames=T)
otc_sv<-read.xlsx('_For testing/NNHPD/Operational Dashboard - pharma OTC data.xlsx',sheet=3,rows=c(59:69),cols=c(1:9),colNames=T)
medical_sv<-read.xlsx('_For testing/TPD/Operational Dashboard - med device data.xlsx',sheet=3,rows=c(122:132),cols=c(1:9),colNames=T)

#-----------------------------------
#load data for information access:
ati<-read.xlsx('_For testing/RMOD/Operational Dashboard - ATI.xlsx',sheet=1,rows=c(2:20),colNames=T)



#-----------------------------------
#load data for historical data

#function to load excel tables
transform_tb<-function(filepath,sheetName,row_start,row_end,levels,level_1,level_2,colN=T){
  
  col1<-enquo(level_1)
  col2<-enquo(level_2)
  
  ds<-read.xlsx(filepath,sheet=sheetName,
                rows=c(row_start:row_end),colNames=colN)
  
  
  colnames(ds)[1:2]<-c('category','X')
  col_selection<-c('category','X','YTD','Current month','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')
  ds<-ds[,grepl(paste(paste0(col_selection,'$'),collapse='|'),colnames(ds))]
  
  ds<-ds%>%
    filter(X %in% levels)%>%
    mutate(category=ifelse(is.na(category),lag(category),category))%>%
    gather(month,count,-category,-X)%>%
    spread(X,count)%>%
    mutate(percent=!!col1/!!col2)
  
  return(ds)
  
}

#function to clean historical data for plotting
clean_ds_forplot<-function(ds){
  
  ds[ds=='NaN']<-NA
  
  ds%>%
    mutate(percent_cat=case_when(percent<0.8 ~'low',
                                 between(percent,0.8,0.9)~'mid',
                                 percent>0.9 ~ 'high'))%>%
    filter(!month %in% c('YTD','Current.month'))%>%
    mutate(month=factor(month,levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')))
  
}
  

#tpd (only tpd provide data on non-cr performance and workload overtime)
tpd_raw_cr<-transform_tb('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',3,4,44,levels=c('On time','Total'),`On time`,Total)
tpd_raw_ncr<-transform_tb('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',3,47,80,levels=c('On time','Total'),`On time`,Total)


#bgtd (#bgtd not providing previous month data, need to store and bind monthly data)
bgtd_raw_cr<-transform_tb('_For testing/BGTD/Operational Dashboard - biologics data.xlsx',3,1,17,levels=c('On time','Total'),`On time`,Total)
#bgtd_raw_ncr<-read.xlsx('_For testing/BGTD/Operational Dashboard - biologics data.xlsx',3,rows=c(19:28),cols=c(1:3),colNames=T)

#medical device
md_raw_cr<-transform_tb('_For testing/TPD/Operational Dashboard - med device data.xlsx',3,1,27,levels=c('On time','Total'),`On time`,Total)
md_raw_ncr<-transform_tb('_For testing/TPD/Operational Dashboard - med device data.xlsx',3,31,58,levels=c('On time','Total'),`On time`,Total)

#food (#food not providing previous month data, need to store and bind monthly data)
food_raw_ncr<-transform_tb('_For testing/FOOD/Operational Dashboard - food data.xlsx',2,1,11,levels=c('On time','Total'),`On time`,Total)

#vdd (#vdd not providing previous month data, need to store and bind monthly data)
vdd_raw_ncr<-transform_tb('_For testing/VDD/Operational Dashboard - vet drugs data.xlsx',2,1,17,levels=c('On time','Total'),`On time`,Total)

#nhp (#nhp not providing previous month data, need to store and bind monthly data)
nhp_raw_ncr<-transform_tb('_For testing/NNHPD/Operational Dashboard - NHP data.xlsx',2,1,13,levels=c('On time','Total'),`On time`,Total)




