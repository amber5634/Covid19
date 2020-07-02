# install and load all necessary packages
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(IRdisplay)
library(leaflet)
library(leafpop)
library(dplyr)
library(plyr)
library(purrr)
library(magrittr)
library(rlang)
library(knitr)
library(anytime)
corona_india <- read.csv("M://Covid19//covid_19_india.csv")
confirmed_color <- "purple"
active_color <- "blue"
cured_color <- "forestgreen"
death_color <- "red"
# Trend line
df_ind <- corona_india %>% 
  dplyr::group_by(State.UnionTerritory) %>% 
  dplyr::summarise(total_confirmed=sum(Confirmed),total_cured=sum(Cured),total_deaths=sum(Deaths)) %>% 
  dplyr::mutate(total_unrecovered = total_confirmed - ifelse(is.na(total_cured), 0, total_cured) - ifelse(is.na(total_deaths), 0, total_deaths)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(States = factor(State.UnionTerritory, levels = State.UnionTerritory))
df_ind_daily <- corona_india %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarise(total_confirmed=sum(Confirmed),total_cured=sum(Cured),total_deaths=sum(Deaths)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(total_active=total_confirmed-total_deaths-total_cured) %>%
  dplyr::mutate(confirmed_cum=cumsum(total_confirmed),
                cured_cum=cumsum(total_cured),
                death_cum=cumsum(total_deaths),
                active_cum=cumsum(total_active))
df_ind_1 <- corona_india %>% dplyr::filter(as.Date(Date) == max(as.Date(Date)))
p2_ind<-plot_ly(data = df_ind_daily) %>%
  add_trace(x = ~ Date,
            y = ~ active_cum,
            type = "scatter",
            mode = "lines+markers",
            name = "Active",
            line = list(color = active_color),
            marker = list(color = active_color)) %>%
  add_trace(x = ~ Date,
            y = ~ cured_cum,
            type = "scatter",
            mode = "lines+markers",
            name = "Recovered",
            line = list(color = cured_color),
            marker = list(color = cured_color)) %>%
  add_trace(x = ~ Date,
            y = ~ death_cum,
            type = "scatter",
            mode = 'lines+markers',
            name = "Death",
            line = list(color = death_color),
            marker = list(color = death_color)) %>%
  add_annotations(x = as.Date("2020-03-01"),
                  y = 42716,
                  text = paste("no. of active cases surpass", "<br>", "the no. of recovered cases"),
                  xref = "x",
                  yref = "y",
                  arrowhead = 5,
                  arrowhead = 3,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -10,
                  ay = 90) %>%
  layout(title = "",
         yaxis = list(title = "Cumulative Number of Cases"),
         xaxis = list(title = "Date"),
         legend = list(x = 0.1, y = 0.9),
         hovermode = "compare")
ggplotly(p2_ind)

# Bar chart for new cases recorded in respective states of India
max_date <- max(as.Date(corona_india$Date))
corona_india %>% 
  dplyr::filter(corona_india$Confirmed, as.Date(Date) == max(as.Date(Date))) %>%
  dplyr::group_by(State.UnionTerritory) %>%
  dplyr::summarise(total_confirmed=sum(Confirmed),total_cured=sum(Cured),total_deaths=sum(Deaths)) %>%
  dplyr::arrange(-total_confirmed,-total_cured,-total_deaths) %>%
  dplyr::mutate(States = factor(State.UnionTerritory, levels = State.UnionTerritory)) %>%
  dplyr::ungroup() %>%
  dplyr::top_n(n = 15, wt = total_confirmed) %>%
  plotly::plot_ly(x = ~ States,
                  y = ~ total_confirmed,
                  text = ~ total_confirmed,
                  textposition = 'auto',
                  type = "bar") %>%
  plotly::layout(yaxis = list(title = "Number of confirmed cases"),
                 xaxis = list(title = ""),
                 margin =  list(
                   l = 10,
                   r = 10,
                   b = 10,
                   t = 10,
                   pad = 2
                 )) 
  