# Libraries
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinydashboard)
# NOTE: In this code ADI and EI refer to the same concept (EI - embedded intervention, ADI - adaptive intervention)
# Analyzed SMART Data
load("smartshinytestdataedit.RData")

# Setting row names as a column, filtering out NA costs, and prepping display columns
binded_data2$ID <- rownames(binded_data2)
binded_data2 <- binded_data2 %>% filter(!is.na(Cost))
binded_data2$CostDisplay <- binded_data2$Cost
binded_data2$CostHover <- binded_data2$Cost
# Getting rid of rows where ID has "vs" in it
binded_data2 <- binded_data2 %>% filter(!grepl("vs", ID, ignore.case = TRUE))

# Setting up the UI with dashboard structure
ui <- dashboardPage(
  # Dashboard header with the title
  dashboardHeader(title = "Interactive Value Efficiency Plot"),
  
  # Sidebar menu for navigation
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Applet", tabName = "applet", icon = icon("bar-chart")),
      menuItem("Github", icon = icon("github"), href = "https://github.com/augustuswgit/Interactive-Value-Efficiency-SMART-Applet/tree/main")
    )
  ),
  
  # Dashboard body with tab content
  dashboardBody(
    # Loading Tailwind and Font Awesome for styling the tables and icons
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
      )
    ),
    
    tabItems(
      # Home tab: placeholder text and the static table
      tabItem(tabName = "home",
              # Centered placeholder info
              div(
                style = "text-align: center; margin-bottom: 20px; font-size: 13px;",
                p("This applet presents a case study for the determination of value efficiency in a sequential multiple
assignment randomized trial (SMART), loosely based on the ENGAGE trial (McKay et al., 2015). Among
the set of alternative intervention versions in an optimization trial, an intervention is value efficient if it is
expected to achieve more preferred effectiveness on the outcome(s) of interest than all alternative
versions that cost the same or less.
In the case study, we summarize the performance of the four embedded interventions in this SMART in
terms of their expected effectiveness and their resource use, defined in terms of the cost to deliver the
intervention. Costs are hypothetical; effectiveness data is simulated, based on real-world parameters
from ENGAGE.
The four alternative embedded interventions provide different courses of motivational interviewing (MI-
IOP, MI-PC, or both), as summarized in the table below [or to the right? Wherever it is]. For more detail,
see Strayhorn et al., Under review.")
              ),
              # Centered static table
              div(
                style = "display: flex; justify-content: center; margin-bottom: 20px;",
                tags$table(
                  style = "border-collapse: collapse; border: 1px solid #999;",
                  tags$thead(
                    tags$tr(
                      tags$th(style = "border:1px solid #999; padding:8px;", "EI"),
                      tags$th(style = "border:1px solid #999; padding:8px;", "Stage 1"),
                      tags$th(style = "border:1px solid #999; padding:8px;", "After 1 month:"),
                      tags$th(style = "border:1px solid #999; padding:8px;", "Stage 2")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "1"),
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "MI-IOP"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "If response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    ),
                    tags$tr(
                      tags$td(style = "border:1px solid #999; padding:8px;", "If non-response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "MI-PC")
                    ),
                    tags$tr(
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "2"),
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "MI-IOP"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "If response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    ),
                    tags$tr(
                      tags$td(style = "border:1px solid #999; padding:8px;", "If non-response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    ),
                    tags$tr(
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "3"),
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "MI-PC"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "If response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    ),
                    tags$tr(
                      tags$td(style = "border:1px solid #999; padding:8px;", "If non-response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "MI-PC")
                    ),
                    tags$tr(
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "4"),
                      tags$td(rowspan = 2, style = "border:1px solid #999; padding:8px;", "MI-PC"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "If response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    ),
                    tags$tr(
                      tags$td(style = "border:1px solid #999; padding:8px;", "If non-response"),
                      tags$td(style = "border:1px solid #999; padding:8px;", "NFC")
                    )
                  )
                )
              )
      ),
      
      # Applet tab: dropdown, plot, sliders, and table
      tabItem(tabName = "applet",
              # Centered dropdown for category selection
              div(
                style = "display: flex; justify-content: center; margin-bottom: 20px;",
                # Hidden dropdown (kept so server logic works)
                tags$div(style = "display:none;",
                         selectInput(
                           inputId = "time_select",
                           label = "Select Category:",
                           choices = c("Time.3","Time.6", "Area.under.Curve"),
                           selected = "Time.3",
                           width = "200px"
                         )
                ),
                
                # Visible replacement text
                h3("Value Efficiency Plot", style = "font-size: 50px;")
              ),
              
              # Row with plot on the left (wide) and controls on the right (narrow)
              fluidRow(
                box(
                  width = 9,
                  plotlyOutput("dot_plot", height = "600px")
                ),
                box(
                  width = 3,
                  h3("Cost Parameters"),
                  sliderInput(
                    inputId = "slider1_plot",
                    label = "Cost to implement MI-IOP",
                    min = 100,
                    max = 600,
                    value = 100,
                    width = "100%"
                  ),
                  sliderInput(
                    inputId = "slider2_plot",
                    label = "Cost to implement MI-PC",
                    min = 100,
                    max = 600,
                    value = 100,
                    width = "100%"
                  ),
                  sliderInput(
                    inputId = "slider3_plot",
                    label = HTML("Switch Cost <span title='Switch cost: The implementation cost incurred by switching from delivering MI-IOP to delivering MI-PC'><i class='fas fa-info-circle'></i></span>"),  # Adding the info icon
                    min = 0,
                    max = 200,
                    value = 0,
                    width = "100%"
                  ),
                  
                  # Static table below the sliders, centered
                  div(
                    style = "display: flex; justify-content: center; margin-top: 20px;",
                    tags$table(
                      class = "table-auto border-collapse border border-gray-400",
                      tags$thead(
                        tags$tr(
                          tags$th("EI", class = "border px-4 py-2"),
                          tags$th("Stage 1", class = "border px-4 py-2"),
                          tags$th("After 1 month:", class = "border px-4 py-2"),
                          tags$th("Stage 2", class = "border px-4 py-2")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td("1", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("MI-IOP", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("If response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("If non-response", class = "border px-4 py-2"),
                          tags$td("MI-PC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("2", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("MI-IOP", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("If response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("If non-response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("3", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("MI-PC", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("If response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("If non-response", class = "border px-4 py-2"),
                          tags$td("MI-PC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("4", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("MI-PC", rowspan = 2, class = "border px-4 py-2"),
                          tags$td("If response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        ),
                        tags$tr(
                          tags$td("If non-response", class = "border px-4 py-2"),
                          tags$td("NFC", class = "border px-4 py-2")
                        )
                      )
                    )
                  )
                )
              )
      )
    )
  )
)

# Server function to handle the logic
server <- function(input, output, session) {
  updateSelectInput(session, "time_select", selected = "Time.3")
  # This reactive filters and calculates costs for the data
  filtered_data <- reactive({
    # Make sure we have a time selection before proceeding
    req(input$time_select)
    
    # Building the pattern based on the selected category
    pattern <- if (input$time_select == "Area.under.Curve") {
      "^Area\\.under\\.Curve"
    } else {
      paste0("^", input$time_select, "\\.")
    }
    
    # Filtering the data by the pattern
    data <- binded_data2 %>%
      filter(grepl(pattern, ID))
    
    # Updating costs and other columns based on ID patterns and sliders
    data <- data %>%
      mutate(
        CostDisplay = case_when(
          grepl("MP", ID) ~ input$slider1_plot + input$slider2_plot + input$slider3_plot,
          grepl("MM", ID) ~ input$slider1_plot,
          grepl("PP", ID) ~ input$slider2_plot * 2,
          grepl("PM", ID) ~ input$slider2_plot,
          TRUE ~ Cost  # Fallback to original if no match
        ),
        CostHover = CostDisplay,
        ADI = case_when(
          grepl("MP", ID) ~ 1,
          grepl("MM", ID) ~ 2,
          grepl("PP", ID) ~ 3,
          grepl("PM", ID) ~ 4,
          TRUE ~ NA_real_
        ),
        CostError = 0  # Starting with zero error
      )
    
    # If it's Time.3 or Time.6, run the Monte Carlo sim for ADI 1 and 3
    if (input$time_select %in% c("Time.3", "Time.6")) {
      set.seed(123)  # Keeping it reproducible
      M <- 1000  # Simulation count
      
      # For ADI 1: response prob and expected cost
      p_r_1 <- rbeta(M, shape1 = 46 + 1, shape2 = 44 + 1)  # Beta for response rate
      p_nr_1 <- 1 - p_r_1
      exp_c_1 <- p_r_1 * input$slider1_plot + p_nr_1 * (input$slider1_plot + input$slider2_plot + input$slider3_plot)
      mean_c_1 <- mean(exp_c_1)  # Average cost
      error_c_1 <- 1.645*sd(exp_c_1)  # Standard deviation as error
      
      # For ADI 3: response prob and expected cost (no switch)
      p_r_3 <- rbeta(M, shape1 = 66 + 1, shape2 = 44 + 1)  # Beta for response rate
      p_nr_3 <- 1 - p_r_3
      exp_c_3 <- p_r_3 * input$slider2_plot + p_nr_3 * (2 * input$slider2_plot)
      mean_c_3 <- mean(exp_c_3)  # Average cost
      error_c_3 <- 1.645*sd(exp_c_3)  # Standard deviation as error
      
      # Applying the simulated values to the data
      data <- data %>% mutate(
        CostDisplay = case_when(
          ADI == 1 ~ mean_c_1,
          ADI == 3 ~ mean_c_3,
          TRUE ~ CostDisplay
        ),
        CostError = case_when(
          ADI == 1 ~ error_c_1,
          ADI == 3 ~ error_c_3,
          TRUE ~ 0
        )
      )
    }
    data
  })
  
  # Reactive to handle plotting logic: colors, lines, dominated points
  plot_data <- reactive({
    data <- filtered_data()
    if (nrow(data) == 0) return(list(data = data, colors = character(0), lines = data.frame()))
    
    # Checking for dominated points
    dominated <- rep(FALSE, nrow(data))
    for (i in 1:nrow(data)) {
      for (j in 1:nrow(data)) {
        if (i == j) next
        if (data$ContrastEstimates[j] >= data$ContrastEstimates[i] &&
            data$CostDisplay[j] <= data$CostDisplay[i] &&
            (data$ContrastEstimates[j] > data$ContrastEstimates[i] | data$CostDisplay[j] < data$CostDisplay[i])) {
          dominated[i] <- TRUE
          break
        }
      }
    }
    
    # Indices of non-dominated points
    non_dom_idx <- which(!dominated)
    
    # Default colors to blue
    colors <- rep("blue", nrow(data))
    lines <- data.frame()
    
    if (length(non_dom_idx) == 1) {
      # Single non-dominated: make it red
      colors[non_dom_idx] <- "red"
    } else if (length(non_dom_idx) > 1) {
      # Sorting non-dominated by x (ContrastEstimates)
      non_dom_data <- data[non_dom_idx, ]
      order_non <- order(non_dom_data$ContrastEstimates)
      non_dom_sorted <- non_dom_data[order_non, ]
      original_idx_sorted <- non_dom_idx[order_non]
      
      # Building the efficient frontier chain (non-decreasing slopes)
      if (nrow(non_dom_sorted) >= 2) {
        chain <- c(1)  # Kick off with the first point
        
        for (i in 2:nrow(non_dom_sorted)) {
          chain <- c(chain, i)
          
          # Loop to prune if slopes decrease
          while (length(chain) >= 3) {
            a <- chain[length(chain) - 2]
            b <- chain[length(chain) - 1]
            c <- chain[length(chain)]
            
            dx1 <- non_dom_sorted$ContrastEstimates[b] - non_dom_sorted$ContrastEstimates[a]
            dy1 <- non_dom_sorted$CostDisplay[b] - non_dom_sorted$CostDisplay[a]
            slope1 <- if (dx1 == 0) Inf else dy1 / dx1  # Avoid div by zero
            
            dx2 <- non_dom_sorted$ContrastEstimates[c] - non_dom_sorted$ContrastEstimates[b]
            dy2 <- non_dom_sorted$CostDisplay[c] - non_dom_sorted$CostDisplay[b]
            slope2 <- if (dx2 == 0) Inf else dy2 / dx2
            
            if (slope2 <= slope1) {
              # Drop the middle point to keep slopes increasing
              chain <- chain[-(length(chain) - 1)]
            } else {
              break
            }
          }
        }
        
        # Purple for the chain points
        selected_chain <- original_idx_sorted[chain]
        colors[selected_chain] <- "purple"
        
        # Connecting chain points with lines if cost increases
        if (length(chain) >= 2) {
          for (k in 1:(length(chain) - 1)) {
            idx1 <- original_idx_sorted[chain[k]]
            idx2 <- original_idx_sorted[chain[k + 1]]
            if (data$CostDisplay[idx2] > data$CostDisplay[idx1]) {
              lines <- rbind(lines, data.frame(
                x = c(data$ContrastEstimates[idx1], data$ContrastEstimates[idx2]),
                y = c(data$CostDisplay[idx1], data$CostDisplay[idx2]),
                group = paste(idx1, idx2)
              ))
            }
          }
          if (nrow(lines) > 0) {
            lines$group <- factor(lines$group)
          }
        }
      }
    }
    
    list(data = data, colors = colors, lines = lines)
  })
  
  # Rendering the main dot plot as Plotly
  output$dot_plot <- renderPlotly({
    plot_data_result <- plot_data()
    data <- plot_data_result$data
    colors <- plot_data_result$colors
    lines <- plot_data_result$lines
    
    # Bail out with a simple message if no data
    if (nrow(data) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    # Building the base ggplot
    p <- ggplot(data, aes(x = ContrastEstimates, y = CostDisplay, color = I(colors), shape = factor(ADI),
                          text = paste(
                            "<b>ID</b>: ", ID, "<br>",
                            "<b>Contrast Estimate</b>: ", round(ContrastEstimates, 2), "<br>",
                            "<b>Cost</b>: ", ifelse(ADI %in% c(2, 4), CostHover, round(CostDisplay, 3)), "<br>",
                            "<b>Std Error</b>: ", round((((ContrastUpper95-ContrastLower95)/2)/1.1913), 3), "<br>", #converted to 90%ci
                            "<b>EI</b>: ", ADI, "<br>",
                            "<b>Cost Error</b>: ", ifelse(ADI %in% c(2, 4), "undefined", round(CostError, 3))
                          ))) +
      geom_point(size = 3) +
      # Horizontal error bars for contrasts
      geom_errorbarh(
        aes(xmin = ContrastEstimates - (((ContrastUpper95-ContrastLower95)/2)/1.1913), # convert to 90% CI
            xmax = ContrastEstimates + (((ContrastUpper95-ContrastLower95)/2))/1.1913),
        height = 10
      ) +
      # Vertical error bars for costs
      geom_errorbar(
        aes(ymin = CostDisplay - CostError,
            ymax = CostDisplay + CostError),
        width = 0.5  # A bit wider for visibility
      ) +
      # Custom shapes for ADI levels
      scale_shape_manual(values = c(15, 16, 17, 18), name = "EI", labels = c("1", "2", "3", "4")) +
      scale_color_identity() +  # Use the colors we set
      labs(
        #title = paste(input$time_select, ": Cost vs Expected Treatment Readiness with Errors"),
        x = "Expected Treatment Readiness at 3 months",
        y = "Total Cost"
      ) +
      scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 8)
      )
    
    # Adding the purple dashed lines if we have any
    if (nrow(lines) > 0) {
      p <- p + geom_line(data = lines, aes(x = x, y = y, group = group),
                         color = "purple", linetype = "dashed", inherit.aes = FALSE)
    }
    
    # Converting to interactive Plotly with hover text
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        showlegend = TRUE,
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
  })
}

# Launching the app
shinyApp(ui = ui, server = server)