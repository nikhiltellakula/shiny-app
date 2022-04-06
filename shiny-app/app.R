library(shiny)
library(dplyr)
library(stringr)
library(shinyHeatmaply)

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
                             h6("WARNING: Computation time. Avoid using DTW with >100 covariates in data set.")),
            
            # Time Series Visualization ----------
            conditionalPanel("input.cov_select == 'Manual'",
                             h3("Visualize the Time Series"),
                             selectizeInput("ts_viz",
                                            "Select the desired time series to plot.",
                                            choices = NULL,
                                            multiple = T)),
            
            # Highlight Target TS ----------
            checkboxInput("ts_highlight",
                          "Highlight the target time series"),
            
            # CI Plot Selection ----------
            selectizeInput("ci_plots",
                           "Select the Causal Impact plots to display.",
                           choices = c("Actual vs Expected" = "original",
                                       "Pointwise" = "pointwise",
                                       "Cumulative" = "cumulative"),
                           selected = c("original",
                                        "pointwise",
                                        "cumulative"),
                           multiple = T),
            
            # Run Causal Impact ----------
            actionButton("run", "Run Causal Impact")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(
                    # Instructions ----------
                    "Instructions",
                    HTML('<ol type="1">
                                 <li>Specify:</li>
                                    <ol type = "a">
                                        <li>the target time series, the the time series upon which we want to infer causal impact;</li>
                                        <li>the date range, the time period of interest;</li>
                                        <li>the intervention date, the date from which causal impact will be analyzed.</li>
                                    </ol>
                                 <li>With the target time series selected, it will be plotted in the <code>Time Series</code> tab.</li>
                                 <li>Select any time series to be explicitly excluded from both manual and automatic covariate selection processes.</li>
                                 <li>Specify the covariate selection process.</li>
                                    <ol type="a">
                                        <li>Manual allows the user to select the covariates of interest based on self-discovery and decisions.</li>
                                        <li>Automatic covariate selection picks the <code>n</code> most highly correlated time series with the target time series over the non-intervention period.</li>
                                        <li>The Dynamic Time Warping checkbox applies a soft dynamic time warping algorithm to calculate correlations among time series based on dates and shape instead of a 1:1 Euclidean date match.</li>
                                    </ol>
                                <li>Once at least a single extra covariate time series has been selected, the <code>Highlight the target time series</code> button will do just that.</li>
                                <li>Once at least a single extra covariate time series has been selected, the correlation heatmap will be populated in the <code>Correlation</code> tab. The covariate time series will also be shown in the <code>Time Series</code> tab.</li>
                                <li>After the desired covariates or method of covariate selection has been chosen, select the types of Causal Impact plots to be output.</li>
                                    <ol type="a">
                                        <li>Actual vs Expected: shows the actual target time series, the predicted, or expected, values for the target time series based on the covariate time series values, and the confidence limits of the predicted values.</li>
                                        <li>Pointwise: the difference between oberved and expected values for the time series with accompanying confidence limits.</li>
                                        <li>Cumulative: the cumulative sum of the difference between observed and expected values during the intervention period.</li>
                                    </ol>
                                <li>Once the parameters have all been selected as desired, click the <code>Run Causal Impact</code> button.</li>
                                <li>The <code>Causal Impact</code> tab contains the causal impact plot coupled with a blurb describing the results of the algorithm. The blurb also explicitly states statistical significance of the intervention.</li>
                                <li><code>Time Series Plot Parameters</code> contains options to modify the time series plot. The title, axis labels, and any text included in the plot can be modified.</li>
                                <li><code>Correlation Heatmap Parameters</code> has a few editable parameters:</li>
                                    <ol type="a">
                                        <li>Color: allows to change the color scheme for the heatmap.</li>
                                        <li>Layout: allows for axis labels and other text modifications.</li>
                                        <li>Dendrogram: the default view for the correlation heatmap orders time series by descending correlations with the target time series. The heatmap can have a dendrogram that automatically clusters the time series. The dendrogram can be useful in order to find clusters of time series for a manual covariate check. The functions used to cluster the time series for the dendrogram are editable.</li>
                                    </ol>
                                <li>The parameters can be changed and modified; just need to click the <code>Run Causal Impact</code> button again.</li>
                              </ol>')
                ),
                tabPanel(
                    # Time Series Visualization ----------
                    "Time Series",
                    plotOutput("ts_plot", width = "100%", height = "800px")
                ),
                tabPanel(
                    # Correlation Heatmap ----------
                    "Correlation",
                    plotlyOutput("heatout", height = "800px")
                ),
                tabPanel(
                    # Causal Impact Visual ----------
                    "Causal Impact Results",
                    plotOutput("ci_plot", width = "100%"),
                    textOutput("ci_report")
                )
            )
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
    
    # Update Covariate Selection ----------
    observe({
        selection <- input$ts_target
        selection2 <- input$ts_exclude
        
        # all variables
        ts_vars <- colnames(data_import())
        ts_vars <- ts_vars[-1]
        
        cov_vars <- ts_vars[!(ts_vars %in% c(selection, selection2))]
        
        updateSelectizeInput(session,
                             "ts_viz",
                             "Select the desired time series to plot.",
                             choices = cov_vars)
    })
    
    output$hat_table <- renderDataTable(data_import())
}

# Run the application 
shinyApp(ui = ui, server = server)
