library(shiny)
library(shinydashboard)
library(dashboardthemes)
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


#load data:
options(digits=3)
bgtd<-read.xlsx('data/Operational Dashboard - biologics data.xlsx',sheet=1,rows=c(3:11),colNames=T)

food<-read.xlsx('data/Operational Dashboard - food data.xlsx',sheet=1,rows=c(3:8),colNames=T)

nhp<-read.xlsx('data/Operational Dashboard - NHP data.xlsx',sheet=1,rows=c(3:9),colNames=T)

med_device<-read.xlsx('data/Operational Dashboard - med device data.xlsx',sheet=1,rows=c(3:14),colNames=T,fillMergedCells = TRUE)

tpd<-read.xlsx('data/Operational Dashboard - pharma rx data.xlsx',sheet=1,rows=c(3:11),colNames=T)
  
mhpd<-read.xlsx('data/Operational Dashboard - post-market data.xlsx',sheet=1,rows=c(3:16),colNames = T)

vet<-read.xlsx('data/Operational Dashboard - vet drugs data.xlsx',sheet=1,rows=c(3:11),colNames = T)
