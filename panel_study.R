#BUNYAMIN PANEL  WORK we will see something publishable in some weeks.
setwd("C:/Users/bunya/rwork")
library(plm)
library(DataExplorer)
library(tidyverse)
library(readxl)
panel <- read.csv("C:/Users/bunya/rwork/lovelycsv.csv")
lovelydata <- read_excel("lovelydata.xlsx")
#choose one way excel or csv
View(lovelydata)
DataExplorer::plot_missing(panel)
panel<- plm.data(panel, index=c("id", "time"))
panel %>% mutate(log_urb = log(urb))
panel %>% mutate(log_pri = log(prisoner))

