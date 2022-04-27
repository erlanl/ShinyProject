library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

master_df <- read.csv('amazon.csv')
#stock_list <- c('AAPL', 'FB', 'GOOG')
stock_list <- c('Acre', 'Amapa', 'Amazonas', 'Para', 'Rondonia', 'Roraima', 'Tocantins')

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Data <- strptime(master_df$Data, format='%Y-%m-%d')