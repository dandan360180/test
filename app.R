library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(leaflet)
library(shinythemes)

### Processed Data for the Map
map <- readOGR("arrondissements.geojson")
pal <- colorNumeric(palette = "RdPu", domain = map$median)
labels <- sprintf("<strong>%s</strong><br/> Median Price: $%g",
                  map$l_aroff, map$median) %>%
    lapply(htmltools::HTML)

### Load airbnb data
load("/Users/macbookproret.t.b/Desktop/R project/apt_data.Rdata")
load("/Users/macbookproret.t.b/Desktop/R project/airbnb_time_serie.Rdata")
load("/Users/macbookproret.t.b/Desktop/R project/num_apt_by_host.Rdata")


### Preparing the Data as needed

price_by_arrond <- apt_features_and_price
visit_by_arrond <- longitudinal
apt_by_host <- count_by_host_1


### Choices for selectInput
arrond_input <- c("Louvre", "Bourse", "Temple", "Hôtel-de-Ville", "Panthéon",
                  "Luxembourg", "Palais-Bourbon", "Élysée", "Opéra", "Entrepôt",
                  "Popincourt", "Reuilly", "Gobelins", "Observatoire",
                  "Vaugirard", "Passy", "Batignolles-Monceau",
                  "Buttes-Montmartre", "Buttes-Chaumont", "Ménilmontant")
rooms <- c(0, 1, 2, 3, 4, 5, 6)
ids <- apt_by_host %>%
    filter(n > 5) %>%
    select(host_id)




### Application Component

### Ui logic
ui <- navbarPage("Paris AirBnB 2009-2016", theme = shinytheme("darkly"),
                 
                 tabPanel("Map and Analysis of AirBnB Data in Paris",
                          leafletOutput("paris", width = "100%", "900px" ),
                          
                          
                          
                          
                          
                          # Floating panel
                          absolutePanel(
                              class     = "panel panel-default",
                              fixed     = TRUE,
                              draggable = TRUE,
                              top       = 60,
                              left      = "auto",
                              right     = 20,
                              bottom    = "auto",
                              width     = "400",
                              height    = "auto",
                              
                              selectInput("arrond",
                                          "Neighbourhood:",
                                          arrond_input,
                                          multiple = FALSE),
                              selectInput("bed",
                                          "Number of bedrooms:",
                                          rooms,
                                          multiple = FALSE),
                              selectInput("bath",
                                          "Number of bathrooms:",
                                          rooms,
                                          multiple = FALSE),
                              plotOutput("price_dist", height = 225),
                              plotOutput("time_visit", height = 170),
                              selectInput("host",
                                          "Hosts_id:",
                                          ids,
                                          multiple = FALSE),
                              plotOutput("apt_by_host", height = 170)
                          )
                 )
)



### Server Logic Component
server <- function(input, output, session) {
    # Create the map
    output$paris <- renderLeaflet({
        leaflet(map) %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            addPolygons(dashArray = "2",
                        color = "black",
                        weight = 2,
                        smoothFactor = 0.2,
                        fillOpacity = 0.5,
                        fillColor = ~pal(median),
                        highlight = highlightOptions(weight = 5,
                                                     color = "white",
                                                     dashArray = "",
                                                     fillOpacity = 0.6,
                                                     bringToFront = TRUE),
                        label = labels) %>%
            addLegend("bottomleft",
                      pal = pal,
                      values = ~median,
                      title = "Median Prices per arrondissement",
                      labFormat = labelFormat(prefix = "$"),
                      opacity = 1)
    })
    
    ## Get and output price data accroding to selected arrondissement
    selected_arrond_price <- reactive({
        neighbourhood <- input$arrond
        bed_num <- input$bed
        bat_num <- input$bath
        total <-  price_by_arrond %>%
            select(price, listing_id)
        selection <- price_by_arrond %>%
            subset(neighbourhood_cleansed == neighbourhood &
                       bedrooms == bed_num & bathrooms == bat_num)
        total %>%
            mutate(key = if_else(listing_id %in% selection$listing_id,
                                 "Sample", "Population"))
    })
    
    
    output$price_dist <- renderPlot({
        (ggplot(selected_arrond_price(), aes(x = price, fill = key))
         + geom_histogram(aes(y = ..density..),
                          colour = "white",
                          alpha = 0.6,
                          bins = 35)
         + geom_density(lty = 2,
                        alpha = 0.3)
         + scale_fill_manual(values = c("#00DD00", "#FA648C"))
         + labs(x = "Price",
                y = "Density\n",
                title = "Distribution by arrondissements")
         + theme_tufte(base_size = 14,
            
                       ticks = TRUE)
         + theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 plot.title = element_text(size = 16, hjust = 2))
         + facet_wrap(~key, nrow = 2))
    })
    
    ## Get and output visit data according to neighbourhood
    selected_arrond_visit <- reactive({
        neighbourhood <- input$arrond
        visit_by_arrond %>%
            subset(neighbourhood_cleansed == neighbourhood)
    })
    
    output$time_visit <- renderPlot({
        (ggplot(selected_arrond_visit(), aes(x = date, y = count_obs, group = 1))
         + geom_line(size = 0.5, colour = "#56DDFF")
         + scale_x_date(date_labels = "%Y")
         + labs(x = "Year",
                y = "# Appartment Rented\n",
                title = "Time-Series Analysis by arrondissements")
         + stat_smooth(color = "#FF5AAC", method = "loess")
         + theme_tufte(base_size = 14,
                       
                       ticks = TRUE)
         + theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 plot.title = element_text(size = 16)))
    })
    
    ## Get and output number of apt by host (superior at 5)
    selected_host <- reactive({
        id <- input$host
        apt_by_host %>%
            subset(host_id == id)
    })
    
    output$apt_by_host <- renderPlot({
        (ggplot(selected_host(), aes(x = "", y = n))
         + geom_col(width = 0.1,
                    colour = "black",
                    fill = "#BE80FF")
         + geom_text(aes(label = n),
                     position = position_dodge(0.9),
                     vjust = 2.5,
                     size = 4,
                     colour = "black")
         + labs(x = "",
                y = "Number of Appartment\n",
                title = "Number of appartments per host")
         + theme_tufte(base_size = 14,
                    
                       ticks = TRUE)
         + theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 plot.title = element_text(size = 16)))
    })
}



shinyApp(ui = ui, server = server)

