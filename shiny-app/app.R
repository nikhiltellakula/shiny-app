library(shiny)
library(dplyr)
library(stringr)

data_df <- readRDS("data-df.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Causal Impact"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Date Range Select ----------
            uiOutput("dates"),
            
            # Intervention Date Select ----------
            uiOutput("int_date")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           "Instructions",
           dataTableOutput("hat_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Data Import Function ----------
    data_import <- reactive({
        df <- data_df
        colnames(df) <- str_replace_all(colnames(df), "[( .)]", "_")
        df <- df %>% mutate(date = as.Date(date))
    })
    
    # Update Date Range ----------
    output$dates <- renderUI({
        min_date <- min(data_import()$date)
        max_date <- max(data_import()$date)
        
        dateRangeInput("date_range",
                       "Specify date range.",
                       start = min_date,
                       end = max_date,
                       min = min_date,
                       max = max_date)
    })
    
    # Add Intervention Date ----------
    output$int_date <- renderUI({
        date <- max(data_import()$date) - 14
        dateInput("intervention",
                  "Specify the intervention date.",
                  value = date)
    })
    
    output$hat_table <- renderDataTable(data_import())
}

# Run the application 
shinyApp(ui = ui, server = server)
