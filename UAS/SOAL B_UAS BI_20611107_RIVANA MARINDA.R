library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Enter Data", tabName = "enter_data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "enter_data",
        fluidPage(
          titlePanel("Enter Data"),
          sidebarLayout(
            sidebarPanel(
              fileInput("file", "Upload CSV File", accept = ".csv"),
              textInput("separator", "Custom Separator (e.g., ';')", value = ","),
              actionButton("loadDataBtn", "Load Data")
            ),
            mainPanel(
              h4("Preview of Loaded Data:"),
              tableOutput("loaded_data_preview"),
              hr(),
              h4("ANOVA Analysis Results:"),
              verbatimTextOutput("anova_results"),
              plotOutput("anova_plot")  # Added plot output
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  loaded_data <- reactiveVal(NULL)
  
  # Enter Data Tab
  observeEvent(input$loadDataBtn, {
    tryCatch({
      req(input$file, input$separator)
      loaded_data(read.csv(input$file$datapath, sep = input$separator))
      
      # Automatically run ANOVA analysis
      run_anova()
    }, error = function(e) {
      showNotification("Error loading data. Please check your file and separator.", type = "error", duration = 5)
    })
  })
  
  output$loaded_data_preview <- renderTable({
    req(loaded_data())
    loaded_data()
  })
  
  # Function to run ANOVA analysis
  run_anova <- function() {
    req(loaded_data())
    
    # Assuming loaded_data() contains appropriate columns for ANOVA
    formula_str <- paste(names(loaded_data())[1], "~", paste(names(loaded_data())[2:ncol(loaded_data())], collapse = " * "))
    model <- aov(as.formula(formula_str), data = loaded_data())
    
    # Display ANOVA summary
    output$anova_results <- renderPrint({
      anova_summary <- summary(model)
      cat("ANOVA Summary:\n")
      print(anova_summary)
      
      # Interpretation of ANOVA results
      p_value <- anova(model)[["Pr(>F)"]][1]
      cat("\nHypothesis Test:\n")
      if (p_value < 0.05) {
        cat("Reject the null hypothesis (H0).\n")
        cat("There is evidence of a significant difference between groups.\n")
      } else {
        cat("Fail to reject the null hypothesis (H0).\n")
        cat("There is not enough evidence to conclude a significant difference between groups.\n")
        cat("Consider the following:\n")
        cat("- Verify that the chosen independent variables are appropriate for the analysis.\n")
        cat("- Check if there are enough observations in each group.\n")
        cat("- Explore other statistical tests or analyses to further investigate relationships.\n")
        cat("- Conduct post-hoc tests if applicable, as ANOVA may not capture pairwise differences.\n")
        cat("- Consult with a statistician or domain expert for further guidance.\n")
      }
    })
    
    # Boxplot to visualize ANOVA results
    output$anova_plot <- renderPlot({
      ggplot(loaded_data(), aes(x = !!sym(names(loaded_data())[2]), y = !!sym(names(loaded_data())[1]))) +
        geom_boxplot() +
        labs(title = "ANOVA Boxplot", x = names(loaded_data())[2], y = names(loaded_data())[1])
    })
  }
}

shinyApp(ui, server)
