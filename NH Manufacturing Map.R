# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)  # To access different color palettes
library(shinyjs)  # For handling JavaScript functions

# Load the CSV file from GitHub repository
data <- read_csv("https://raw.githubusercontent.com/your-username/NH-MFG-Map-App/main/www/Sample%20Set%203.17.25.csv")

# Define UI
ui <- fluidPage(
  # Add a graphic banner from your GitHub repository
  tags$head(
    tags$style(HTML("
      .header-graphic {
        width: 100%;
        height: auto; /* Adjusts the height of the image automatically */
        background-color: #f0f0f0; /* Optional: Light gray background */
        text-align: center;
        padding: 20px;
      }
      .header-graphic img {
        width: 100%;
        height: auto;
        object-fit: contain;
      }
    "))
  ),
  
  # Header section with the image banner from GitHub
  div(
    class = "header-graphic",
    img(src = "https://raw.githubusercontent.com/your-username/NH-MFG-Map-App/main/www/NH%20Manufacturing%20Interactive%20Search%20Map%20for%20Supply%20Chain%20Inventory.jpg", 
        class = "header-graphic")
  ),
  
  # Title Panel
  titlePanel("Search Company by Keyword and NAICS Code"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Search Keyword:", value = ""),
      br(),
      helpText("Type a keyword to search in Company Name, Industry Category, or Description"),
      br(),
      
      # Buttons for Select All and Unselect All
      actionButton("select_all", "Select All", class = "btn-primary"),
      actionButton("unselect_all", "Unselect All", class = "btn-danger"),
      br(),
      br(),
      
      # Checkbox group for Industry Categories (alphabetized)
      checkboxGroupInput(
        "industry_filter", 
        "Select Industry Categories:", 
        choices = sort(unique(data$`Industry Category`)),  # Sort industry categories alphabetically
        selected = sort(unique(data$`Industry Category`)),  # Default selection is all categories
        inline = FALSE
      ),
      
      # Text Input for NAICS Code search (positioned under Industry Categories)
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

# Define Server Logic
server <- function(input, output, session) {
  
  # Filtered data based on selected industry categories (and optionally, the keyword or NAICS code)
  filtered_data <- reactive({
    # Step 1: Filter by selected industry categories
    req(input$industry_filter)  # Ensure that industry categories are selected
    filtered <- filter(data, `Industry Category` %in% input$industry_filter)
    
    # Step 2: Apply the keyword filter if it's provided
    if (input$keyword != "") {
      filtered <- filtered %>%
        filter(
          grepl(input$keyword, `Company Name`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Category`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Sub Category`, ignore.case = TRUE) |
            grepl(input$keyword, `NAICS 2022 Description`, ignore.case = TRUE)
        )
    }
    
    # Step 3: Apply the NAICS code filter if it's provided
    if (input$naics_code != "") {
      filtered <- filtered %>%
        filter(grepl(input$naics_code, `NAICS 2022 Code`, ignore.case = TRUE))
    }
    
    return(filtered)
  })
  
  # Create a color palette for Industry Categories
  industry_palette <- reactive({
    req(filtered_data())  # Ensure filtered data is available
    # Use "Set1" for a more vibrant set of colors (you can change it to other palettes)
    colorFactor(palette = brewer.pal(9, "Set1"), domain = unique(filtered_data()$`Industry Category`))
  })
  
  # Render Leaflet Map with colored circle markers
  output$map <- renderLeaflet({
    req(filtered_data())  # Ensure there's filtered data
    
    # Get the color palette function
    color_fn <- industry_palette()
    
    # Create the leaflet map with circle markers for each company
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste("<b>", `Company Name`, "</b><br>",
                       `Industry Category`, "<br>", `NAICS 2022 Description`, "<br>",
                       "<a href='", URL, "' target='_blank'>Website</a>"),
        color = ~color_fn(`Industry Category`),  # Assign color based on Industry Category
        radius = 8,  # Set the radius size of the circle markers
        stroke = FALSE,  # Remove border from circle markers
        fillOpacity = 0.8  # Set opacity for the fill color
      )
  })
  
  # Render the search results table with colored Industry Category text
  output$search_results <- renderTable({
    req(filtered_data())  # Ensure there's filtered data
    
    # Get the color palette function
    color_fn <- industry_palette()
    
    # Create a function to apply color to the Industry Category text
    filtered_data() %>%
      mutate(IndustryCategoryColor = sapply(`Industry Category`, function(x) {
        color_fn(x)
      })) %>%
      select(`CIS ID#`, `Industry Category`, `Industry Sub Category`, `Company Name`, `NAICS 2022 Code`, IndustryCategoryColor) %>%
      # Use a custom HTML to color the Industry Category text
      { 
        # Return the data frame and apply custom HTML to the Industry Category column
        data.frame(
          `CIS ID#` = .$`CIS ID#`,
          `Industry Category` = paste0('<span style="color:', .$IndustryCategoryColor, '">', .$`Industry Category`, '</span>'),
          `Industry Sub Category` = .$`Industry Sub Category`,
          `Company Name` = .$`Company Name`,
          `NAICS 2022 Code` = .$`NAICS 2022 Code`
        )
      }
  }, sanitize.text.function = function(x) x)  # Prevent escaping of HTML in the table
  
  # Observe events for Select All and Unselect All buttons
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = sort(unique(data$`Industry Category`)))  # Select all categories
  })
  
  observeEvent(input$unselect_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = character(0))  # Deselect all categories
  })
}

# Run the application
# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)  # To access different color palettes
library(shinyjs)  # For handling JavaScript functions

# Load the CSV file from GitHub repository
data <- read_csv("https://raw.githubusercontent.com/your-username/NH-MFG-Map-App/main/www/Sample%20Set%203.17.25.csv")

# Define UI
ui <- fluidPage(
  # Add a graphic banner from your GitHub repository
  tags$head(
    tags$style(HTML("
      .header-graphic {
        width: 100%;
        height: auto; /* Adjusts the height of the image automatically */
        background-color: #f0f0f0; /* Optional: Light gray background */
        text-align: center;
        padding: 20px;
      }
      .header-graphic img {
        width: 100%;
        height: auto;
        object-fit: contain;
      }
    "))
  ),
  
  # Header section with the image banner from GitHub
  div(
    class = "header-graphic",
    img(src = "https://raw.githubusercontent.com/your-username/NH-MFG-Map-App/main/www/NH%20Manufacturing%20Interactive%20Search%20Map%20for%20Supply%20Chain%20Inventory.jpg", 
        class = "header-graphic")
  ),
  
  # Title Panel
  titlePanel("Search Company by Keyword and NAICS Code"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Search Keyword:", value = ""),
      br(),
      helpText("Type a keyword to search in Company Name, Industry Category, or Description"),
      br(),
      
      # Buttons for Select All and Unselect All
      actionButton("select_all", "Select All", class = "btn-primary"),
      actionButton("unselect_all", "Unselect All", class = "btn-danger"),
      br(),
      br(),
      
      # Checkbox group for Industry Categories (alphabetized)
      checkboxGroupInput(
        "industry_filter", 
        "Select Industry Categories:", 
        choices = sort(unique(data$`Industry Category`)),  # Sort industry categories alphabetically
        selected = sort(unique(data$`Industry Category`)),  # Default selection is all categories
        inline = FALSE
      ),
      
      # Text Input for NAICS Code search (positioned under Industry Categories)
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

# Define Server Logic
server <- function(input, output, session) {
  
  # Filtered data based on selected industry categories (and optionally, the keyword or NAICS code)
  filtered_data <- reactive({
    # Step 1: Filter by selected industry categories
    req(input$industry_filter)  # Ensure that industry categories are selected
    filtered <- filter(data, `Industry Category` %in% input$industry_filter)
    
    # Step 2: Apply the keyword filter if it's provided
    if (input$keyword != "") {
      filtered <- filtered %>%
        filter(
          grepl(input$keyword, `Company Name`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Category`, ignore.case = TRUE) |
            grepl(input$keyword, `Industry Sub Category`, ignore.case = TRUE) |
            grepl(input$keyword, `NAICS 2022 Description`, ignore.case = TRUE)
        )
    }
    
    # Step 3: Apply the NAICS code filter if it's provided
    if (input$naics_code != "") {
      filtered <- filtered %>%
        filter(grepl(input$naics_code, `NAICS 2022 Code`, ignore.case = TRUE))
    }
    
    return(filtered)
  })
  
  # Create a color palette for Industry Categories
  industry_palette <- reactive({
    req(filtered_data())  # Ensure filtered data is available
    # Use "Set1" for a more vibrant set of colors (you can change it to other palettes)
    colorFactor(palette = brewer.pal(9, "Set1"), domain = unique(filtered_data()$`Industry Category`))
  })
  
  # Render Leaflet Map with colored circle markers
  output$map <- renderLeaflet({
    req(filtered_data())  # Ensure there's filtered data
    
    # Get the color palette function
    color_fn <- industry_palette()
    
    # Create the leaflet map with circle markers for each company
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste("<b>", `Company Name`, "</b><br>",
                       `Industry Category`, "<br>", `NAICS 2022 Description`, "<br>",
                       "<a href='", URL, "' target='_blank'>Website</a>"),
        color = ~color_fn(`Industry Category`),  # Assign color based on Industry Category
        radius = 8,  # Set the radius size of the circle markers
        stroke = FALSE,  # Remove border from circle markers
        fillOpacity = 0.8  # Set opacity for the fill color
      )
  })
  
  # Render the search results table with colored Industry Category text
  output$search_results <- renderTable({
    req(filtered_data())  # Ensure there's filtered data
    
    # Get the color palette function
    color_fn <- industry_palette()
    
    # Create a function to apply color to the Industry Category text
    filtered_data() %>%
      mutate(IndustryCategoryColor = sapply(`Industry Category`, function(x) {
        color_fn(x)
      })) %>%
      select(`CIS ID#`, `Industry Category`, `Industry Sub Category`, `Company Name`, `NAICS 2022 Code`, IndustryCategoryColor) %>%
      # Use a custom HTML to color the Industry Category text
      { 
        # Return the data frame and apply custom HTML to the Industry Category column
        data.frame(
          `CIS ID#` = .$`CIS ID#`,
          `Industry Category` = paste0('<span style="color:', .$IndustryCategoryColor, '">', .$`Industry Category`, '</span>'),
          `Industry Sub Category` = .$`Industry Sub Category`,
          `Company Name` = .$`Company Name`,
          `NAICS 2022 Code` = .$`NAICS 2022 Code`
        )
      }
  }, sanitize.text.function = function(x) x)  # Prevent escaping of HTML in the table
  
  # Observe events for Select All and Unselect All buttons
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = sort(unique(data$`Industry Category`)))  # Select all categories
  })
  
  observeEvent(input$unselect_all, {
    updateCheckboxGroupInput(session, "industry_filter", selected = character(0))  # Deselect all categories
  })
}

# Run the application
shinyApp(ui = ui, server = server)

