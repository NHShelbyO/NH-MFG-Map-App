# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(shinyjs)

# Load the CSV file directly from GitHub using raw URL
data_url <- "https://raw.githubusercontent.com/NHShelbyO/NH-MFG-Map-App/main/Sample%20Set%203.17.25.csv"
data <- read_csv(data_url)

# Define UI
ui <- fluidPage(
  # JavaScript to hide keyboard when tapping outside text inputs
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

  # Replace with your GitHub-hosted image (via raw link)
  div(
    class = "header-graphic",
    img(src = "https://raw.githubusercontent.com/NHShelbyO/NH-MFG-Map-App/main/www/NH%20Manufacturing%20Interactive%20Search%20Map%20for%20Supply%20Chain%20Inventory.jpg")
  ),

  titlePanel("Search Company by Keyword and NAICS Code"),

  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Search Keyword:", value = ""),
      helpText("Type a keyword to search in Company Name, Industry Category, or Description"),
      br(),
      actionButton("select_all", "Select All", class = "btn-primary"),
      actionButton("unselect_all", "Unselect All", class = "btn-danger"),
      br(), br(),
      checkboxGroupInput(
        "industry_filter",
        "Select Industry Categories:",
        choices = sort(unique(data$`Industry Category`)),
        selected = sort(unique(data$`Industry Category`))
      ),
      textInput("naics_code", "Search by NAICS Code:", value = ""),
      helpText("Enter NAICS code to filter companies by NAICS description")
    ),

    mainPanel(
      leafletOutput("map"),
      br(),
      tableOutput("search_results")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$industry_filter)
    filtered <- filter(data, `Industry Category` %in% input$industry_filter)

    if (input$keyword != "") {
      filtered <- filtered %>%
        filter(
          grepl(input$keyword, `Company Name`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Category`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Sub Category`, ignore.case = TRUE) |
            grepl(input$keyword, `NAICS 2022 Description`, ignore.case = TRUE)
        )
    }

    if (input$naics_code != "") {
      filtered <- filtered %>%
        filter(grepl(input$naics_code, `NAICS 2022 Code`, ignore.case = TRUE))
    }

    return(filtered)
  })

  industry_palette <- reactive({
    req(filtered_data())
    colorFactor(palette = brewer.pal(9, "Set1"), domain = unique(filtered_data()$`Industry Category`))
  })

  output$map <- renderLeaflet({
    req(filtered_data())
    color_fn <- industry_palette()

    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste("<b>", `Company Name`, "</b><br>",
                       `Industry Category`, "<br>", `NAICS 2022 Description`, "<br>",
                       "<a href='", URL, "' target='_blank'>Website</a>"),
        color = ~color_fn(`Industry Category`),
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })

  output$search_results <- renderTable({
    req(filtered_data())
    color_fn <- industry_palette()

    filtered_data() %>%
      mutate(IndustryCategoryColor = sapply(`Industry Category`, function(x) {
        color_fn(x)
      })) %>%
      select(`CIS ID#`, `Industry Category`, `Industry Sub Category`, `Company Name`, `NAICS 2022 Code`, IndustryCategoryColor) %>%
      {
        data.frame(
          `CIS ID#` = .$`CIS ID#`,
          `Industry Category` = paste0('<span style="color:', .$IndustryCategoryColor, '">', .$`Industry Category`, '</span>'),
          `Industry Sub Category` = .$`Industry Sub Category`,
          `Company Name` = .$`Company Name`,
          `NAICS 2022 Code` = .$`NAICS 2022 Code`
        )
      }
  }, sanitize.text.function = function(x) x)

  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = sort(unique(data$`Industry Category`)))
  })

  observeEvent(input$unselect_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = character(0))
  })
}

# Run app
shinyApp(ui = ui, server = server)
