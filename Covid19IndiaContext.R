# Load the libraries
library(tidyverse)
library(knitr)
library(plotly)
library(DT)

#Load the data
data<-read.csv("covid_19_india.csv", stringsAsFactors = FALSE)

# to display the internal structure (ideally onle line for each basic structure )
str(data)

# Data format
data$Date <- parse_datetime(data$Date, format = "%d/%m/%y")

# Quickly check first few rows and columns
head(data)

# Grouping the data date wise and using summarise function 
# On more details about summarise enter ? summarise on console and hit enter

temp_df <- data %>% group_by(Date) %>%
  summarise(Confirmed_Cases = sum(Confirmed),
            Recovered_Cases = sum(Cured),
            Death_Cases = sum(Deaths))

# Plot for Confirmed Cases

temp_df %>% plot_ly(x = ~Date, y = ~Confirmed_Cases,
                    type = "scatter", mode = "line", name = "Confirmed Cases") %>% 
  layout(yaxis = list(title = "Number of Confirmed Cases"))

# Plot for Recovered Cases 

temp_df %>% plot_ly(x = ~Date, y = ~Recovered_Cases, type = "scatter", mode = "line", name = "Recovered Cases") %>% 
  layout(yaxis = list(title = "Number of Recovered Cases"))

# Plot for Deceased Cases
temp_df %>% plot_ly(x = ~Date, y = ~Death_Cases, type = "scatter", mode = "line", name = "Deceased Cases") %>% 
  layout(yaxis = list(title = "Number of Deceased Cases"))

# Top effected States

top_states <- data %>% group_by(State.UnionTerritory) %>%
  summarise(Confirmed_Cases = sum(Confirmed),
            Recovered_Cases = sum(Cured),
            Death_Cases = sum(Deaths)) %>% arrange(-Confirmed_Cases) %>% head(10)

# Plot for most effected states in India

top_states %>% plot_ly(x = ~State.UnionTerritory, y = ~Confirmed_Cases, type = "bar", color = ~State.UnionTerritory) %>% 
  layout(showlegend = F, xaxis = list(title = "States"), yaxis = list(title = "Confirmed Cases"))


# For interactive datatable
datatable(data[ , -c(1,3)], caption = "Interactive Data Table",
          rownames = T,
          filter = "top",
          options = list(pageLength = 25))

