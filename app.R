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
library(plotly)
library(osmdata)
library(tmap)
library(eurostat)
library(plotly)
library(glue)
library(showtext)
library(bslib)
library(thematic)

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
standorte_valid <- df[!duplicated(df$standort),"standort"][['standort']] #alle standorte
standorte_all <- standorte_valid
vars <- as.data.frame(x = names(df)[4:8]) %>% rename("Schadstoff" = "names(df)[4:8]") # alle Variablen
vars <- vars[["Schadstoff"]]

# Define UI for application 
ui <- fluidPage(
    theme = my_theme,
    titlePanel("Airquality"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider", label = "Select the range of the displayed data.", min = min(df$datum), max = max(df$datum), value = c(min(df$datum), max(df$datum))),
            selectInput("select_site", "Select a site for the data.", choices = standorte_valid, selected = "München/Landshuter Allee"),
            checkboxGroupInput("select_vars", "Select a site for the data.", choices = vars, selected = "stickstoffmonoxid"),
            ),
        mainPanel(
            plotOutput("first_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    calculate_valid_standorte <- function(){
        temp_df <- df
        for(i in standorte_all){
            x <- temp_df %>% 
                filter(standort == i) %>%
                select(input$select_vars)%>%
                summarise(sum(!is.na(.))) 
            if(x == 0){
                temp_df <- temp_df %>%
                    filter(standort != i)
            }
        }
        standorte_valid <- temp_df[!duplicated(temp_df$Standort),"Standort"][['standort']]
        return(standorte_valid)
    }
    
    has_emission <- function(){
        x <- df %>% 
            filter(standort ==  input$select_site) %>%
            select(input$select_vars)%>%
            summarise(sum(!is.na(.))) 
        x >= 1
    }

    standorte_valid <- reactive({
        calculate_valid_standorte()
    })
    
    output$first_plot <- renderPlot({
        
        scale <- function(x, na.rm = TRUE) (x / first(na.omit(x)))
        
        validate(
            need(input$select_vars, "Check at least one emission"),
            need(has_emission(), "Emission not available for this site")
        )
        df %>%
            filter(standort == input$select_site) %>%
            filter(datum >= input$slider[1] & datum <= input$slider[2] ) %>%
            select(c(standort, datum, input$select_vars)) %>%
            mutate_at(input$select_vars, scale)%>%
            pivot_longer(cols =  input$select_vars, names_to = "variables") %>%
            ggplot(aes(datum, value, color = variables)) +
            geom_line(size = 0.5) +
            labs(title = glue::glue("Veränderung der {str_to_title(input$select_vars)}-Emissionen"),
                 y = "Veränderung relativ zum Beginn", x = "Datum")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
