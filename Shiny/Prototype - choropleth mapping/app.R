pacman::p_load(shiny, tidyverse, sf)

mpsz <- st_read(dsn = "data/geospatial",
                layer="MP14_SUBZONE_WEB_PL")

pop <- read_csv("data/aspatial/respopagesextod2011to2020.csv")
print(pop)

popdata2020 <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15]))%>%
  mutate(`AGED`=rowSums(.[16:21])) %>%
  mutate(`TOTAL`=rowSums(.[3:21])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`)

popdata2020 <- popdata2020 %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>%
  filter(`ECONOMY ACTIVE` > 0)


mpsz_pop2020 <- left_join(mpsz, popdata2020,
                          by = c("SUBZONE_N" = "SZ"))


ui <- fluidPage(
  titlePanel("Choropleth mapping system"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="variable",  # needs to be unique value
        label = "Mapping variable",
        choices = list("aged" = "AGED",
                    "young" = "YOUNG",
                    "economy active" = "ECONOMY ACTIVE",
                    "dependency" = "DEPENDENCY"),
        selected = "DEPENDENCY",
        multiple = TRUE
      ),
      selectInput(
        inputId = "classification",
        label = "Classification method",
        choices = list("sd" = "sd", 
                       "equal" = "equal",  
                       "pretty" = "pretty", 
                       "quantile" = "quantile", 
                       "kmeans" = "kmeans", 
                       "hclust" = "hclust", 
                       "bclust" = "bclust", 
                       "fisher" = "fisher", 
                       "jenks" = "jenks" )
      ),
      sliderInput(
        inputId = "classes",
        label = "Number of classes",
        min = 6,
        max = 12,
        value = c(6) 
      ),
      selectInput(
        inputId = "colour",
        label = "Colour scheme",
        choices = list("blues" = "Blues", 
                       "reds" = "Reds",  
                       "greens" = "Greens", 
                       "yellow-orangs-red" = "T1Or-Rd", 
                        ),
        selected = "Y1OrRd"
      ),
      mainPanel(
        "mapPlot",
        width = "100%",
        height = 400
      )
    )
  )
)



server <- function(input, output){
  output$mapPlot <- renderTmap({
    tm_shape(mpsz_pop2020)+
      tm_polygons("DEPENDENCY")
  })
}

shinyApp(ui = ui, server = server)

