library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(broom)

master_df <- read.csv('amazon_correct.csv')
stock_list <- c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", "Espirito Santo",
                "Goias", "Maranhao", "Mato Grosso", "Minas Gerais", "Para", "Paraiba", "Pernambuco", "Piau", "Rio",
                "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins")
time_list <- c("1998", "1999","2000","2001", "2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")


master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Data <- strptime(master_df$Data, format='%Y')