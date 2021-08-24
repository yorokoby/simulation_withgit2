



library(shiny)
library(tidyverse)
library(rlang)
library(sf)
library(tmap)



# prepare data -------------------------------------------------------------

data<-read_csv(here::here("data","FSNAU_proc.csv"))
shapes<-sf::st_read("./shapefile", "Som_Admbnda_Adm2_UNDP")

shapes1<-shapes%>%
  rename(regions=admin1Name,
         districts=admin2Name)

data1<-data%>%
  full_join(.,shapes1)%>%
  select(year,month,regions,districts,rainfall,price_maize,geometry)%>%
  mutate(rainfall=as.numeric(rainfall),
         price_maize=as.numeric(price_maize))%>%
  st_as_sf()

data_names<-data1%>%
  select(-month,-year,-regions,-districts)%>%
  st_drop_geometry()

# static ------------------------------------------------------------------

#data1%>%
  #filter(year==2019,
           #month==4,
           #regions=="Bakool")%>%
  #ggplot()+
  #geom_histogram(aes(x=as.numeric(rainfall)),fill="grey",colour = "black")


data1%>%
  filter(year==2019,
         month==4,
         regions=="Bakool")%>%
  tm_shape() +
  tm_borders()+
  tm_fill(col="rainfall",legend.show = TRUE,palette="Blues",contrast=c(0.2,1))

# dashboard ---------------------------------------------------------------


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Histogram",
             titlePanel("Histogram"),
             fluidRow(column(9, plotOutput("histogram")),
                      column(3,selectInput(inputId = "year_hist",label = "select year",choices=2010:2019)),
                      column(3,selectInput(inputId = "month_hist",label = "select month",choices=1:12)),
                      column(3,selectInput(inputId = "regions_hist",label = "select region",choices=unique(data1$regions))),
                      column(3,selectInput(inputId = "variable_hist",label = "select variable",choices=names(data_names)))
                      )
    ),
    
    
    tabPanel("Maps",
                 titlePanel("Maps"),
                 fluidRow(column(9, plotOutput("maps")),
                          column(3,selectInput(inputId = "year_map",label = "select year",choices=2010:2019)),
                          column(3,selectInput(inputId = "month_map",label = "select month",choices=1:12)),
                          column(3,selectInput(inputId = "regions_map",label = "select region",choices=unique(data1$regions))),
                          column(3,selectInput(inputId = "variable_map",label = "select variable",choices=names(data_names)))
                 )
    )
    
   
  )
  
)


server <- function(input, output, session) {
  
  year_hist <- reactive({
    input$year_hist
  })
  
  month_hist <- reactive({
    input$month_hist
  })
  
  regions_hist <- reactive({
    input$regions_hist
  })
  
 
  output$histogram<-renderPlot(data1%>%
    filter(year==year_hist(),
           month==month_hist(),
           regions==regions_hist())%>%
    ggplot()+
    geom_histogram(aes(x=as.numeric(.data[[input$variable_hist]])),fill="grey",colour = "black"))
  
  
  
  
  
  year_map <- reactive({
    input$year_map
  })
  
  month_map <- reactive({
    input$month_map
  })
  
  regions_map <- reactive({
    input$regions_map
  })
  
  
  
  variable_map <- reactive({
    input$variable_map
  })
  
  
  
  
  
  
  
  
  output$maps<-renderPlot(data1%>%
    filter(year==year_map(),
           month==month_map(),
           regions==regions_map())%>%
    tm_shape() +
    tm_borders()+
    tm_fill(col=variable_map(),legend.show = TRUE,palette="Blues",contrast=c(0.2,1)))
  
  
  
  
  
  
  
}




shinyApp(ui, server)

