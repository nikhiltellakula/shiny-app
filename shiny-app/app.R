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
            h3("Dates"),
            uiOutput("dates"),
            
            # Intervention Date Select ----------
            uiOutput("int_date"),
            
            # Target Time Series ----------
            h3("Target Time Series"),
            uiOutput("ts_target"),
            
            # Exclusion Time Series ----------
            selectizeInput("ts_exclude",
                           "Select time series to exclude from covariate selection.",
                           choices = NULL,
                           multiple = T),
            
            # Covariate Selection Process ----------
            radioButtons("cov_select",
                         "Covariate Selection Process",
                         c("Manual", "Automatic"),
                         inline = T),
            
            # Number of Covariates ----------
            conditionalPanel("input.cov_select == 'Automatic'",
                             uiOutput("num_covariates"),
                             checkboxInput("dtw",
                                           "Use Dynamic Time Warping"),
                             h6("WARNING: Computation time. Avoid using DTW with >100 covariates in data set."))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           "Instructions",
           dataTableOutput("hat_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
    
    # Target Time Series ----------
    output$ts_target <- renderUI({
        
        # all variables
        ts_vars <- colnames(data_import())
        ts_vars <- ts_vars[-1] # get rid of date
        
        selectizeInput("ts_target",
                       "Select the target time series.",
                       choices = ts_vars,
                       multiple = F)
    })
    
    # Exclusion Time Series ----------
    observeEvent(input$ts_target, {
        selection <- input$ts_target
        
        # all variables
        ts_vars <- colnames(data_import())
        ts_vars <- ts_vars[-1] # remove date variable
        
        cov_vars <- ts_vars[ts_vars != selection]
        
        updateSelectizeInput(session,
                             "ts_exclude",
                             "Select time series to exclude from covariate selection.",
                             choices = cov_vars)
    })
    
    # Update Number of Covariates ----------
    output$num_covariates <- renderUI({
        num_covs <- length(colnames(data_import())) - 2 - length(input$ts_exclude)
        sliderInput("covs",
                    "Covariates to be input to Causal Impact Model",
                    min = 1, max = num_covs, value = 1)
    })
    
    output$hat_table <- renderDataTable(data_import())
}

# Run the application 
shinyApp(ui = ui, server = server)
