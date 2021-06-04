#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(readr)
library(sf)
library(osmdata)
library(tmap)
library(eurostat)
library(plotly)
library(glue)
library(showtext)
library(bslib)
library(thematic)
library(shinydashboard)

# Setup Theme
my_theme <- bs_theme(bootswatch = "simplex",
                     base_font = font_google("Fira Sans"))

# thematic updates the plots and fonts to match the "my_theme"-theme
thematic_shiny(font = "auto")

# import data
df <- read_csv("data/schadstoffe-zeitreihen.csv")
geo_data <- read_rds("data/geo_schadstoffe.rds") %>%
    inner_join(df, by = "standort_id")  %>%
    select(c("standort_id", "geometry"))
geo_data <-geo_data[!duplicated(geo_data$standort_id), ] # considerably faster than distinct()
standorte <- df[!duplicated(df$standort),"standort"] #alle standorte
vars <- as.data.frame(names(df)[4:11]) # alle Variablen

# Define UI for application 
ui <- fluidPage(
    theme = my_theme,
    titlePanel("Airquality"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider", label = "Select the range of the displayed data.", min = min(df$datum), max = max(df$datum), value = c(min(df$datum), max(df$datum))),
            selectInput("select_site", "Select a site for the data.", choices = standorte, selected = "München/Landshuter Allee"),
            selectInput("select_vars", "Select a site for the data.", choices = vars, selected = "kohlenstoffmonoxid"),
            ),
        mainPanel(
            plotOutput("first_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$first_plot <- renderPlot({
        scale <- function(x, na.rm = TRUE) (x / first(na.omit(x)))
        
        df %>%
            filter(standort == input$select_site) %>%
            filter(datum >= input$slider[1] & datum <= input$slider[2] ) %>%
            select(c(standort, datum, input$select_vars)) %>%
            mutate_at(input$select_vars, scale)%>%
            pivot_longer(cols =  input$select_vars, names_to = "variables") %>%
            ggplot(aes(datum, value, color = variables)) +
            geom_line(size = 0.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
