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
tpd_ncr<-read.xlsx('_For testing/TPD/Operational Dashboard - pharma rx data.xlsx',sheet=1,rows=c(3:15),colNames=T)
  
mhpd<-read.xlsx('_For testing/MHPD/Operational Dashboard - post-market data.xlsx',sheet=1,rows=c(3:16),colNames = T)

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