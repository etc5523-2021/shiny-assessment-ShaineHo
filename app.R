library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)

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
            "Hide countries < 1 million population",
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
                                        "\nPopulation: {scales::number(total_population, big.mark = ",")}",
                                        "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                        "\nGDP per capita: {scales::dollar(gdp_per_capita)}")

# Define reactive expressions here for filtering data

countrysize <- reactive({
if (input$small_countries) {
  tidy_fuels %>%
   filter(total_population > 1000000)
}
  else {
  tidy_fuels
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

date_input <- reactive({
  countryname() %>%
    filter(year %in% input$year)

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
      h <- highlight_key(date_input(), ~country)
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
             y = "Proportion of access to clean fuels for cooking",
             color = "Continent")+
        scale_color_manual(values = continent.color)+
        theme_bw()
      ggplotly(p, tooltip = "text")%>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")) %>%
        highlight(on = "plotly_hover", off = "plotly_doubleclick")
      }

    else{
      h <- highlight_key(date_input(), ~country)
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
        labs(title = "Global access to clean fuels in 2016",
             x = "Logged GDP per captia",
             y = "Proportion of access to clean fuels for cooking",
             color = "")+
        scale_color_manual(values = continent.color)+
        theme_bw()

      ggplotly(p, tooltip = "text") %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")) %>%
        highlight(on = "plotly_hover", off = "plotly_doubleclick")
    }
  })

  output$table <- renderDataTable({
    countryname() %>%
      select(-tooltip, -code) %>%
      mutate(gdp_per_capita = scales::number(gdp_per_capita, accuracy = .01, big.mark = ","),
             total_population = scales::number(total_population, big.mark = ",")) %>%
      arrange(country, desc(year)) %>%
    datatable(options = list(pageLength = 10,
                             scrollX = TRUE,
                             auto_browse(TRUE)
                             ))
  })
}

runApp(shinyApp(ui, server))
