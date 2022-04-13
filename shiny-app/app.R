# 1. Libraries -------------------------------------------------------------
library(dtw)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(shinyHeatmaply)

# 2. Raw Data Input  -------------------------------------------------------
data_df <- readRDS("data-df.rds")

# 3. Necessary Functions  --------------------------------------------------
logplus <- function(x) {
    sapply(x, function(x) {
        if (x <= 0) {
            return(0.00)
        } else {
            return(log(x))
        }
    })
}

create_market_vectors <- function(data, test_market, ref_market) {
    
    # remove missing values
    d <- data %>% filter(!is.na(value))
    
    # filter for the market of interest
    test <- d %>% 
        filter(time_series == test_market) %>% 
        select(date, value) %>% 
        rename("y" = "value")
    
    # grab data of the reference markets
    if (length(ref_market) == 1) {
        ref <- d %>% 
            filter(time_series == ref_market[1]) %>% 
            select("date", "value") %>% 
            rename("x1" = "value") %>% 
            inner_join(test, by = "date")
        
        return(list(as.numeric(ref$y),
                    as.numeric(ref$x1),
                    as.Date(ref$date)))
        
    } else if (length(ref_market) > 1) {
        d <- d %>% distinct(time_series, date, .keep_all = TRUE)
        ref <- reshape2::dcast(subset(d, time_series %in% ref_market),
                               date ~ time_series,
                               value.var = "value")
        names(ref) <- c("date", paste0("x", seq(1:length(ref_market))))
        f <- test %>% inner_join(ref, by = "date") %>% drop_na()
        
        return(list(as.numeric(f$y),
                    dplyr::select(f, num_range("x", 1:length(ref_market))),
                    as.Date(f$date)))
    }
}

calculate_distances <- function(market_to_be_matched, data,
                                warping_limit, dtw_emphasis) {
    
    messages <- 0
    
    # list of all time series that are not market of interest
    other_series <- unique(data$time_series)
    
    # empty data frame to store all the values
    distances <- data.frame(matrix(nrow = length(other_series) - 1,
                                   ncol = 10))
    names(distances) <- c("time_series", "BestControl", "RelativeDistance",
                          "Correlation", "Length", "SUMTEST", "SUMCNTL",
                          "RAWDIST", "Correlation_of_logs", "populated")
    distances$populated <- 0
    
    # define column to show market of interest
    distances$time_series <- market_to_be_matched
    
    # remove the market of interest from the vector
    potential_covariates <- other_series[!other_series == market_to_be_matched]
    distances$BestControl <- potential_covariates
    
    # loop through all markets to find dtw distance
    for (j in 1:length(potential_covariates)) {
        
        isValidTest <- TRUE
        ThatMarket <- potential_covariates[j]
        row <- which(startsWith(distances$BestControl, ThatMarket))
        
        mkts <- create_market_vectors(data, market_to_be_matched, ThatMarket)
        
        test <- mkts[[1]]
        ref <- mkts[[2]]
        dates <- mkts[[3]]
        sum_test <- NA
        sum_cntl <- NA
        dist <- 0
        
        # If insufficient data or no variance
        if ((stats::var(test) == 0 | 
             length(test) <= 2 * warping_limit + 1) | 
            sum(abs(test)) == 0) {
            isValidTest <- FALSE
            messages <- messages + 1
        }
        
        # good enough data for dtw
        if (market_to_be_matched != ThatMarket &
            isValidTest == TRUE &
            var(ref) > 0 &
            length(test) > 2 * warping_limit + 1) {
            sum_test <- abs(sum(test))
            sum_cntl <- abs(sum(ref))
            
            if (dtw_emphasis > 0 & sum_test > 0) {
                # actually calculate the distance between time series
                rawdist <- dtw(test,
                               ref,
                               window.type = sakoeChibaWindow,
                               window.size = warping_limit)$distance 
                dist <- rawdist / sum_test
            } else if (dtw_emphasis == 0) {
                dist <- 0
                rawdist <- 0
            } else {
                dist <- -1000000000
                rawdist <- -1000000000
            }
            
            # update the data frame
            distances[row, "Correlation"] <- cor(test, ref)
            distances[row, "populated"] <- 1
            distances[row, "RelativeDistance"] <- dist
            distances[row, "Skip"] <- FALSE
            distances[row, "Length"] <- length(test)
            distances[row, "SUMTEST"] <- sum_test
            distances[row, "SUMCNTL"] <- sum_cntl
            distances[row, "RAWDIST"] <- rawdist
            
            if (max(ref) > 0 & max(test) > 0) {
                distances[row, "Correlation_of_logs"] <- cor(logplus(test),
                                                             logplus(ref))
            } else {
                distances[row, "Correlation_of_logs"] <- -1000000000
            }
        } else {
            if (market_to_be_matched != ThatMarket) {
                messages <- messages + 1
                distances[row, "Skip"] <- TRUE
                if (dtw_emphasis == 0) {
                    distances[row, "RelativeDistance"] <- 0
                    distances[row, "RAWDIST"] <- 0
                } else {
                    distances[row, "RelativeDistance"] <- -1000000000
                    distances[row, "RAWDIST"] <- -1000000000
                }
                
                # update the data frame
                distances[row, "populated"] <- 1
                distances[row, "Correlation"] <- -1000000000
                distances[row, "Length"] <- 0
                distances[row, "SUMTEST"] <- 0
                distances[row, "SUMCNTL"] <- 0
                distances[row, "Correlation_of_logs"] <- -1000000000
            }
        }
    }
    
    if (messages > 0) {
        cat(paste0(messages,
                   " markets were not matched with ",
                   market_to_be_matched,
                   " due to insufficient data or no variance."))
        cat("\n")
        cat("\n")
    }
    
    distances$w <- dtw_emphasis
    distances$MatchingStartDate <- min(data$date)
    distances$MatchingEndDate <- max(data$date)
    
    # filter down to only the top matches
    distances <- distances %>% 
        filter(populated == 1) %>% 
        mutate(dist_rank = rank(RelativeDistance),
               corr_rank = rank(-Correlation),
               combined_rank = w + dist_rank + (1 - w) + corr_rank) %>% 
        arrange(combined_rank) %>% 
        select(-dist_rank, -combined_rank, -corr_rank, -populated, -w) %>% 
        mutate(rank = row_number(),
               NORMDIST = if_else(
                   SUMTEST + SUMCNTL > 0 & !(RAWDIST %in% c(-1000000000, 0)),
                   2 * RAWDIST / (SUMTEST + SUMCNTL), -1000000000
               ),
               NORMDIST = na_if(NORMDIST, -1000000000),
               NORMDIST = na_if(NORMDIST, 0),
               RAWDIST = na_if(RAWDIST, -1000000000),
               RAWDIST = na_if(RAWDIST, 0))
    
    if (dtw_emphasis == 0 & nrow(distances) > 0) {
        distances$RelativeDistance <- NA
    }
    
    return(distances)
    
}

best_matches <- function(data, matches, market_to_be_matched,
                         start_match_period, end_match_period) {
    
    # filter the data between the dates
    data <- data %>% 
        filter(date >= as.Date(start_match_period) & date <= as.Date(end_match_period))
    
    # calculate the distances
    all_distances <- calculate_distances(market_to_be_matched, data, 1, 0.5)
    
    # filter to desired number
    distances <- all_distances %>% filter(rank <= matches)
    
    return(distances)
}

# 4. Shiny App Itself  -----------------------------------------------------
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
                             checkboxInput("dtw_button",
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
    
    # Create TS Data Frame ----------
    ts_df <- reactive({
        # shiny requirements
        req(input$date_range)
        validate(need(!is.na(input$date_range[1]) & !is.na(input$date_range[2]),
                      "Error: Please provide both a start and end date."))
        validate(need(input$date_range[1] < input$date_range[2],
                      "Error: Start date should be earlier than end date."))
        validate(need(input$intervention < input$date_range[2] & 
                          input$intervention > input$date_range[1],
                      "Error: Intervention Date must be between start and end dates."))
        
        # remove the excluded time series
        df_no_excl <- data_import() %>% 
            select(-input$ts_exclude)
        
        # begin data processing
        if (input$cov_select == "Manual") {
            df <- df_no_excl %>% 
                select(date, input$ts_target, input$ts_viz) %>% 
                filter(date >= input$date_range[1] & date <= input$date_range[2]) %>% 
                pivot_longer(cols = c(input$ts_target, input$ts_viz))
        } else if (input$cov_select == "Automatic") {
            # Dynamic Time Warping
            if (input$dtw_button == 1) {
                # all time series long instead of wide
                dtw_df <- df_no_excl %>% 
                    pivot_longer(!date,
                                 names_to = "time_series",
                                 values_to = "value")
                
                # find matched markets for each time series
                mm <- best_matches(data = dtw_df,
                                   matches = input$covs,
                                   market_to_be_matched = input$ts_target,
                                   start_match_period = input$date_range[1],
                                   end_match_period = input$intervention)

                best_covariates <- mm$BestControl
                
                # filter for selected data
                df <- df_no_excl %>% 
                    select(date, input$ts_target, best_covariates) %>% 
                    filter(date >= input$date_range[1] & date <= input$date_range[2]) %>% 
                    pivot_longer(cols = c(input$ts_target,
                                          best_covariates))
                
            } else if (input$dtw_button == 0) {
                # no dynamic time warping
                df <- df_no_excl %>% 
                    # only want to correlate date range of interest
                    filter(date >= input$date_range[1] & date < input$intervention) %>% 
                    select(-date) %>% 
                    select(input$ts_target, everything())
                correlation <- cor(df)
                correlation[is.na(correlation)] <- 0
                correlation <- correlation[order(correlation[, 1],
                                                 decreasing = T), ]
                
                # grabbing the automatic desired time series
                num_ts <- input$covs + 1
                ts_of_interest <- correlation[1:num_ts, ]
                top <- rownames(ts_of_interest)
                
                # filter the data for TS listed in `top`
                df <- df_no_excl %>% 
                    select(date, all_of(top)) %>% 
                    filter(date >= input$date_range[1] & date <= input$date_range[2]) %>% 
                    pivot_longer(cols = all_of(top))
            }
        }
        df
    })
    
    # Time Series Visualization ----------
    time_series_plot <- reactive({
        if (input$ts_highlight == 0) {
            p <- ggplot(ts_df(), aes(x = date, y = value, color = name)) +
                geom_line() +
                theme_bw() +
                labs(x = "Date Value",
                     y = "Response Value",
                     color = "Time Series") +
                geom_vline(xintercept = input$intervention,
                           linetype = "twodash",
                           color = "black",
                           size = 1.5)
        } else if (input$ts_highlight == 1) {
            p <- ggplot(ts_df(), aes(x = date, y = value)) +
                geom_line(aes(color = name == input$ts_target,
                              size = name == input$ts_target,
                              group = name)) +
                theme_bw() +
                labs(x = "Date Value",
                     y = "Response Value") +
                geom_vline(xintercept = input$intervention,
                           linetype = "twodash",
                           color = "black",
                           size = 1.5) +
                scale_color_manual(name = "Time Series",
                                   labels = c("Covariates", input$ts_target),
                                   values = c("grey50", "red")) +
                scale_size_manual(name = "Time Series",
                                  labels = c("Covariates", input$ts_target),
                                  values = c(0.5, 2))
        }
        p
    })
    
    # Render Time Series Plot ----------
    observeEvent(input$ts_target, {
        output$ts_plot <- renderPlot({
            time_series_plot()
        })
    })
    
    # Create COR Data Frame ----------
    cor_df <- reactive({
        df <- ts_df() %>%
            pivot_wider() %>% 
            filter(date < input$intervention) %>% 
            select(-date)
        df <- cor(df)
        df[is.na(df)] <- 0
        
        # sort by the target column -- most correlated time series
        df <- df[order(df[, 1], decreasing = T), ]
        to_order <- rownames(df)
        df <- subset(df, select = to_order)
        df
    })
    
    # Correlation Heatmap ----------
    interactive_heatmap <- reactive({
        df <- cor_df()
        
        # dendrogram clustering parameters
        distfun_row <- function(x) {
            dist(x, method = input$distFun_row)
        }
        distfun_col <- function(x) {
            dist(x, method = input$distFun_col)
        }
        
        hclustfun_row <- function(x) {
            hclust(x, method = input$hclustFun_row)
        }
        hclustfun_col <- function(x) {
            hclust(x, method = input$hclustFun_col)
        }
        
        # actual plot code
        p <- heatmaply(
            df,
            colors = RdBu(256),
            main = "Correlation Heatmap",
            xlab = "Time Series",
            ylab = "Time Series",
            row_text_angle = 0,
            column_text_angle = 45,
            dendrogram = "none"
            # colors = eval(parse(text = paste0(input$pal, "(", input$ncol, ")"))),
            # main = input$main_hm,
            # xlab = input$xlab_hm,
            # ylab = input$ylab_hm,
            # row_text_angle = input$row_text_angle,
            # column_text_angle = input$column_text_angle,
            # dendrogram = input$dendrogram,
            # branches_lwd = input$branches_lwd,
            # seriate = input$seriation,
            # scale = input$scale,
            # distfun_row = distfun_row,
            # distfun_col = distfun_col,
            # hclustfun_row = hclustfun_row,
            # hclustfun_col = hclustfun_col,
            # k_col = input$c,
            # k_row = input$r
        ) %>% 
            layout(margin = list(l = input$l,
                                 b = input$b,
                                 r = "0px"))
        p
    })
    
    # Render Heatmap ----------
    observeEvent(input$ts_target, {
        output$heatout <- renderPlotly({
            interactive_heatmap()
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
