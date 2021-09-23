library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(shinyWidgets)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data/cooking.csv"))
# you might want to use highlight_key here

ui <- fluidPage(
  title = "Indoor Air Pollution",
  tabsetPanel(
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow(
        column(
          2,
          radioGroupButtons("linear_scale",
                            "Linear/Log Selector",
                            choices = c("LINEAR", "LOG"),
                            selected = "LINEAR",
                            justified = TRUE)
        ),
        column(
          6,
          offset = 1,
          # also possible to use plotly here
        ),
        column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million population",
            value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year", "Year",
        min = 2000,
        max = 2016,
        value = c(2000,2016),
        sep = "",
        width = "100%"
      )

    ),
    tabPanel("table", dataTableOutput("table"), icon = icon("table")),
    tabPanel("about", icon = icon("question"))
  )
)

server <- function(input, output, session) {

  tidy_fuels$tooltip <- glue::glue_data(tidy_fuels,
                                        "Country: {country}",
                                        "\nPopulation: {scales::number(total_population, big.mark = ",")}",
                                        "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                        "\nGDP per capita: {scales::dollar(gdp_per_capita)}")

# Define reactive expressions here for filtering data

  min <- reactive({input$year[1]})
  max <- reactive({input$year[2]})

  year_input <- reactive({
    tidy_fuels %>%
      filter(year >= min(), year <= max())

  })

countrysize <- reactive({
if (input$small_countries) {
  year_input() %>%
   filter(total_population > 1000000)
}
  else {
  year_input()
}

})
countryname <- reactive({
  if (is.null(input$countries)){
  countrysize()
}
  else {
  countrysize() %>%
      filter(country %in% input$countries)
}

})


  # Define outputs here
  output$chart <- renderPlotly({
  # Set the color based on Olympics Rings
    continent.color <- c("Asia" = "#FCD200",
                         "Europe" = "#056FCD",
                         "Africa" = "#616060",
                         "Oceania" = "#069A21",
                         "South America" = "#EE0C0C",
                         "North America" = "#FF8181")
    if(input$linear_scale == "LINEAR"){
      p <- countryname() %>%
        highlight_key(~country, "Select Countries") %>%
         ggplot(aes(x = gdp_per_capita,
                   y = cooking)) +
        geom_point(aes(text = tooltip,
                       label = country,
                       size = total_population,
                       color = continent))+
        scale_size_continuous(labels = ~scales::number(.,
                                                       scale = 1/1e6,
                                                       accuracy = 1,
                                                       big.mark = ",",
                                                       suffix = "million")) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_x_continuous(labels = scales::dollar_format(prefix = "$",big.mark = ","))+
        labs(title = "Global access to clean fuel from 2000-2016",
             x = "GDP per captia",
             y = "Access to clean fuels and technologies for cooking",
             color = "Continent")+
        scale_color_manual(values = continent.color)+
        theme_minimal()

      highlight(ggplotly(p, tooltip = "text"),
                selectize = TRUE,
                persistent = TRUE)%>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))

      }

    else{
      p <- countryname() %>%
        highlight_key(~country, "Select Countries") %>%
        ggplot(aes(x = gdp_per_capita,
                   y = cooking)) +
        geom_point(aes(text = tooltip,
                       label = country,
                       size = total_population,
                       color = continent)) +
        scale_size_continuous(labels = ~scales::number(.,
                                                       scale = 1/1e6,
                                                       accuracy = 1,
                                                       big.mark = ",",
                                                       suffix = "million")) +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_x_log10(labels = ~scales::dollar(., accuracy = 1),
                      breaks = c(1000, 2000, 5000, 10000, 20000, 100000))+
        labs(title = "Global access to clean fuels from 2000-2016",
             x = "Logged GDP per captia",
             y = "Proportion of access to clean fuels for cooking",
             color = "")+
        scale_color_manual(values = continent.color)+
        theme_minimal()

      highlight(ggplotly(p, tooltip = "text"),
                selectize = TRUE,
                persistent = TRUE)%>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))
    }
  })

  output$table <- renderDataTable({
    countryname() %>%
      select(-tooltip, -code) %>%
      mutate(gdp_per_capita = scales::dollar(gdp_per_capita,
                                             aclassccuracy = .01,
                                             big.mark = ","),
             total_population = scales::number(total_population,
                                               scale = 1e-6,
                                               accuracy = 0.01,
                                               suffix = " Million"),
             cooking = scales::percent(cooking,
                                       scale = 1,
                                       accuracy = 0.01)) %>%

      arrange(desc(year)) %>%
    datatable(colnames = c("Country" = "country",
                           `Access to clean fuels and technologies for cooking` = "cooking",
                           `GDP per capita (int.-$)` = "gdp_per_capita",
                           "Population" = "total_population"),
              options = list(pageLength = 10,
                             scrollX = TRUE
              ))
  })
}

runApp(shinyApp(ui, server))
