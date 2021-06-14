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
library(leaflet)
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

##data for the map
sf <- eurostat::get_eurostat_geospatial(output_class = "sf", nuts_level = 1)
sf_germany <- sf %>% filter(CNTR_CODE=="DE") 
map_data <-geo_data %>%
    left_join(df)
map_data <- map_data[!duplicated(map_data$standort_id), c("gebiet", "standort", "hoehe", "typ")]%>%
    mutate(content = glue::glue(paste(sep = "<br/>",
                                      "<b><a>{standort}</a></b>",
                                      "Höhe: {hoehe}m",
                                      "Typ: {typ}")))


# Define UI for application 
ui <- fluidPage(
    theme = my_theme,
    titlePanel("Airquality"),
    radioButtons("current_theme", "App Theme:", c("Light" = "simplex", "Dark" = "slate")),
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider", label = "Select the range of the displayed data.", min = min(df$datum), max = max(df$datum), value = c(min(df$datum), max(df$datum))),
            selectInput("select_site", "Select a site for the data.", choices = standorte_valid, selected = "München/Landshuter Allee"),
            checkboxGroupInput("select_vars", "Select a site for the data.", choices = vars, selected = "stickstoffmonoxid"),
            ),
        mainPanel(
            tabsetPanel(
                tabPanel("Zeitreihen", 
                         plotlyOutput("first_plot")
                         ),
                
                tabPanel("Standorte", 
                         plotlyOutput("map"),
                         selectInput("select_type", "Select a variable to classify the sites.", 
                                     choices = c("gebiet", "typ", "hoehe") )
                         ),
                tabPanel("Leaflet",
                         leafletOutput("mymap")
                         )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    standorte_valid <- reactive({
        temp_df <- df
        for(i in standorte_all){
            for(v in input$select_vars){
                s <- temp_df %>% 
                filter(standort == i) %>%
                select(v)%>%
                summarise(sum(!is.na(.))) 
                if(s == 0){
                    temp_df <- temp_df %>%
                        filter(standort != i)
                }
            }
            
        }
        standorte_valid = temp_df[!duplicated(temp_df$standort),"standort"][['standort']]
        updateSelectInput(session, "select_site",
                          choices = standorte_valid,
                          selected = head(standorte_valid,1))
        return(standorte_valid)
    })
    
    # check if the dataset has this emission
    # not necessary anymorge thanks to updated choices
    has_emission <- function(){
        if(length(input$select_vars) == 0){
            return (TRUE)
        } 
        x <- df %>% 
            filter(standort ==  input$select_site) %>%
            select(input$select_vars)%>%
            summarise(sum(!is.na(.))) 
        x >= 1
    }
    
    
    
    output$first_plot <- renderPlotly({
        
        scale <- if(length(input$select_vars) > 1){
            function(x, na.rm = TRUE) (x / first(na.omit(x)))   # function to scale the plotted data
        }
        else{
            function(x, na.rm = TRUE) (x / 1)
        }
        standorte_valid()     # update input choices of sites
        
        
        # validation to have the user to check at least one emission
        validate(
            need(input$select_vars, "Check at least one emission!")
            #need(has_emission(), "Emission not available for this site. Sorry.")
        )
        
        
        ggplotly(df %>%
            filter(standort == input$select_site) %>%
            filter(datum >= input$slider[1] & datum <= input$slider[2] ) %>%
            select(c(standort, datum, input$select_vars)) %>%
            mutate_at(input$select_vars, scale) %>%
            pivot_longer(cols =  input$select_vars, names_to = "variables") %>%
            ggplot(aes(datum, value, color = variables)) +
            geom_line(size = 0.5) +
            labs(title = glue::glue("Veränderung der {str_to_title(input$select_vars)}-Emissionen"),
                 y = "Veränderung relativ zum Beginn", x = "Datum")+
            theme_minimal()
        )
    })
    
    output$map <- renderPlotly({
        p <- sf_germany %>%
            ggplot() +
            geom_sf() +
            geom_sf(data = map_data, size = 2, mapping = aes(text = standort, color = .data[[input$select_type]]))
        ggplotly(p)
    })
    
    output$mymap <- renderLeaflet({
        m <- leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(data = map_data$geometry, label = lapply(map_data$content, HTML)) 
        m
    })
    
    observe({
        # Make sure theme is kept current with desired
        session$setCurrentTheme(
            bs_theme_update(my_theme, bootswatch = input$current_theme)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
