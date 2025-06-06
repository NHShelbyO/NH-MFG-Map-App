## Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(shinyjs)
library(DT)

# Load the CSV file directly from GitHub using raw URL
data_url <- "https://raw.githubusercontent.com/NHShelbyO/NH-MFG-Map-App/main/R_Map_Data_EOMay.csv"
data <- read_csv(data_url)

# Define UI
ui <- fluidPage(
  # JavaScript to hide keyboard on mobile when tapping outside inputs
  tags$script(HTML("
    document.addEventListener('touchstart', function(event) {
      var isTextInput = event.target.tagName === 'INPUT' || event.target.tagName === 'TEXTAREA';
      if (!isTextInput) {
        var inputs = document.querySelectorAll('input, textarea');
        inputs.forEach(function(input) {
          input.blur();
        });
      }
    }, false);
  ")),

  useShinyjs(),

  tags$head(
    tags$style(HTML("
      .header-graphic {
        width: 100%;
        height: auto;
        background-color: #f0f0f0;
        text-align: center;
        padding: 0px;
      }
      .header-graphic img {
        max-width: 100%;
        height: auto;
        object-fit: contain;
      }
    "))
  ),

  div(
    class = "header-graphic",
    img(src = "https://raw.githubusercontent.com/NHShelbyO/NH-MFG-Map-App/main/www/NH%20Manufacturing%20Interactive%20Search%20Map%20for%20Supply%20Chain%20Inventory.jpg")
  ),

  titlePanel("Search Company by Keyword and NAICS Code"),

  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Search Keyword:", value = ""),
      textInput("naics_code", "Search NAICS Code:", value = ""),
      helpText("Type a keyword to search in Company Name, Industry Category, Subcategory, Description, or Keywords."),
      br(),
      actionButton("select_all", "Select All", class = "btn-primary"),
      actionButton("unselect_all", "Unselect All", class = "btn-danger"),
      br(), br(),
      selectInput(
        inputId = "industry_filter",
        label = "Select Industry Categories:",
        choices = sort(unique(data$`Industry Category`)),
        selected = sort(unique(data$`Industry Category`)),
        multiple = TRUE,
        selectize = TRUE
      )
    ),

    mainPanel(
      leafletOutput("map"),
      br(),
      DTOutput("search_results")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$industry_filter)
    filtered <- filter(data, `Industry Category` %in% input$industry_filter)

    if (input$keyword != "") {
      keyword <- tolower(input$keyword)
      filtered <- filtered %>%
        filter(
          grepl(keyword, tolower(`Company Name`)) |
          grepl(keyword, tolower(`Industry Category`)) |
          grepl(keyword, tolower(`Industry Sub Category`)) |
          grepl(keyword, tolower(`NAICS 2022 Description`)) |
          grepl(keyword, tolower(`Key Words/ Phrases`))
        )
    }

    if (input$naics_code != "") {
      filtered <- filtered %>%
        filter(grepl(input$naics_code, `NAICS 2022 Code`, ignore.case = TRUE))
    }

    return(filtered)
  })

  industry_palette <- reactive({
    colorFactor(palette = brewer.pal(9, "Set1"), domain = unique(data$`Industry Category`))
  })

  output$map <- renderLeaflet({
    req(filtered_data())
    color_fn <- industry_palette()

    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste0(
          "<b>", `Company Name`, "</b><br>",
          `Industry Category`, " / ", `Industry Sub Category`, "<br>",
          `NAICS 2022 Description`, "<br>",
          "<a href='", URL, "' target='_blank'>Website</a><br>",
          "<small>", `Key Words/ Phrases`, "</small>"
        ),
        color = ~color_fn(`Industry Category`),
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })

  output$search_results <- renderDT({
    req(filtered_data())
    color_fn <- industry_palette()

    filtered_data() %>%
      mutate(IndustryCategoryColor = sapply(`Industry Category`, color_fn)) %>%
      mutate(`Industry Category` = paste0(
        '<span style="color:', IndustryCategoryColor, '">', `Industry Category`, '</span>'
      )) %>%
      select(`Industry Category`, `Industry Sub Category`, `Company Name`, `NAICS 2022 Code`, `NAICS 2022 Description`) %>%
      datatable(escape = FALSE, options = list(pageLength = 10))
  })

  observeEvent(input$select_all, {
    updateSelectInput(session, "industry_filter", selected = sort(unique(data$`Industry Category`)))
  })

  observeEvent(input$unselect_all, {
    updateSelectInput(session, "industry_filter", selected = character(0))
  })
}

# Run app
shinyApp(ui = ui, server = server)
