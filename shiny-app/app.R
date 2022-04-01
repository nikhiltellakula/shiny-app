#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)

data_df <- readRDS("data-df.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Causal Impact"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
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
        df
    })
    
    output$hat_table <- renderDataTable(data_import())
}

# Run the application 
shinyApp(ui = ui, server = server)
