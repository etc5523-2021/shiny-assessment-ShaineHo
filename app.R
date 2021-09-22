library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
# you might want to use highlight_key here

ui <- fluidPage(
  title = "Indoor Air Pollution",
  tabsetPanel(
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow(
        column(
          2,
          checkboxInput("linear_scale",
            "Linearize x-axis",
            value = FALSE
          )
        ),
        column(
          6,
          offset = 1,
          # also possible to use plotly here
          selectizeInput("countries", "Select Countries",
            choices = c(tidy_fuels$country),
            multiple = TRUE
          )
        ),
        column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million",
            value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year", "Year",
        min = 2000,
        max = 2016,
        value = 2016,
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
                                        "\nPopulation: {scales::label_number_auto()(total_population)}",
                                        "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                        "\nGDP per capita: {scales::dollar(gdp_per_capita)}")

# Define reactive expressions here for filtering data
date_input <- reactive({
  tidy_fuels %>%
    filter(year %in% input$year)
  })

countrysize <- reactive({
if (input$small_countries) {
  date_input() %>%
   filter(total_population > 1000000)
} else {
  date_input()
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
                         "Africa" = "black",
                         "Oceania" = "#069A21",
                         "South America" = "#EE0C0C",
                         "North America" = "#FF8181")
    if(input$linear_scale){
      h <- highlight_key(countryname(), ~country)
      p <- h %>%
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
        scale_x_continuous(labels = scales::dollar_format(prefix = "$",big.mark = ","))+
        labs(title = "Global access to clean fuel",
             x = "GDP per captia",
             y = "Access to clean fuels and technologies for cooking",
             color = "Continent")+
        scale_color_manual(values = continent.color)+
        theme_bw()
      ggplotly(p, tooltip = "text")
      }

    else{
      h <- highlight_key(countryname(), ~country)
      p <- h %>%
        ggplot(aes(x = log(gdp_per_capita),
                   y = cooking
        )) +
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
        scale_x_continuous(labels = scales::dollar_format(prefix = "$"))+
        labs(title = "Global access to clean fuel in 2016",
             x = "Logged GDP per captia",
             y = "Access to clean fuels and technologies for cooking",
             color = "")+
        scale_color_manual(values = continent.color)+
        theme_bw()

      ggplotly(p, tooltip = "text")
    }
  })

  output$table <- renderDataTable({
    countryname() %>%
      select(-year, -tooltip, -code) %>%
      mutate(gdp_per_capita = scales::number(gdp_per_capita, accuracy = .01, big.mark = ","),
             total_population = scales::number(total_population, big.mark = ",")) %>%

    datatable(options = list(pageLength = 10,
                             scrollX = TRUE,
                             auto_browse(TRUE)
                             ))
  })
}

runApp(shinyApp(ui, server))
