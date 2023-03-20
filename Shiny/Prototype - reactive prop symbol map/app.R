
pacman::p_load(shiny, tidyverse, sf)

sgpools <- read_csv("data/aspatial/SGPools_svy21.csv")
sgpools_sf <- st_as_sf(sgpools, 
                       coords= c("XCOORD",
                                 "YCOORD"),
                       crs = 3414)

ui <- fluidPage(
  titlePanel("Reactive Proportional Symbol map"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="type",
        label = "Branch or Outlet",
        choices = c("branch" = "Branch",
                    "outlet" = "Outlet"),
        selected = "Branch",
        multiple = TRUE
      ),
      sliderInput(
        inputId = "winning",
        label = "Number of wins",
        min = 5,
        max = 82,
        value = 20
      ),
      checkboxInput(
        inputId = "showData",
        label = "Show data table",
        value = TRUE
      )
    ),
    mainPanel(
      tmapOutput("mapPlot"),
      DT::dataTableOutput(outputId = "aTable")
    )
  )
)

server <- function(input, output){
  dataset <- reactive({
    sgpools_sf %>%
      filter(`OUTLET TYPE` %in% input$type) %>%
      filter(`Gp1Gp2 Winnings` >= input$winning)
  })
  
  output$mapPlot <- renderTmap({
    tm_shape(shp= dataset(),
             bbox= st_bbox(sgpools_sf)) +
      tm_bubbles(col = "OUTLET TYPE",
                 size = "Gp1Gp2 Winnings",
                 border.col = "black",
                 border.lwd = 0.5)+
      tm_view(set.zoom.limits = c(11,16))
    
  })
  
  # select - display the selected columns starting from name col onwards
  output$aTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(data = dataset() %>%
                      select(1:4),
                    options = list(pageLenth = 10),
                    rownames = FALSE)
    }
  })
}
shinyApp(ui = ui, server = server)


