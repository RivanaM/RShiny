library(shiny)
library(shinydashboard)
library(corrplot)
library(lmtest)
library(nortest)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Multiple Linear Regression Estimation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Enter Data", tabName = "enter_data", icon = icon("table")),
      menuItem("Variable Correlation", tabName = "correlation", icon = icon("chart-line")),
      menuItem("Regression", tabName = "regression", icon = icon("line-chart"))
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
              tableOutput("loaded_data_preview")
            )
          )
        )
      ),
      tabItem(
        tabName = "correlation",
        fluidPage(
          titlePanel("Variable Correlation"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("correlation_variable_selector")
            ),
            mainPanel(
              fluidRow(
                column(12, h4("Correlation Plot:")),
                column(12, plotOutput("correlation_tab_plot")),
                HTML("<p style='font-size:14px;'>Plot korelasi pada analisis regresi linier berganda memberikan gambaran visual tentang hubungan antara variabel-variabel. Scatter plot membantu identifikasi hubungan linear, deteksi outlier, dan memberikan pandangan tentang kekuatan dan arah korelasi. Nilai korelasi sendiri memberikan ukuran kuantitatif sejauh mana dua variabel berkaitan. Nilai positif/negatif menunjukkan arah korelasi, sementara nilai dekat 1 atau -1 menunjukkan hubungan yang kuat. Jika plot tampak linier positif, maka dapat dilanjutkan analisis dengan model regresi linier berganda</p>")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "regression",
        fluidPage(
          titlePanel("Multiple Linear Regression Estimation"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("dependent_selector"),
              uiOutput("independent_selector"),
              actionButton("estimateBtn", "Estimate Regression"),
              hr(),
              textOutput("regression_equation"),
              uiOutput("prediction_input_selector"),
              actionButton("predictBtn", "Make Prediction")
            ),
            mainPanel(
              fluidRow(
                column(12, h4("Instructions:")),
                column(12, verbatimTextOutput("backward_instructions")),
                column(12, h4("Regression Coefficients:")),
                column(12, verbatimTextOutput("coefficients")),
                column(12, h4("Summary:")),
                column(12, verbatimTextOutput("summary")),
                column(12, h4("Regression Tests:")),
                column(12, textOutput("autokorelasi_test_regression")),
                column(12, textOutput("homoskedastisitas_test_regression")),
                column(12, textOutput("ks_test_regression")),
                column(12, h4("Regression Plot:")),
                column(12, plotOutput("regression_plot"))
              ),
              fluidRow(
                column(12, h4("Residual Plot:")),
                column(12, plotOutput("residual_plot"))
              ),
              fluidRow(
                column(12, h4("QQ Plot:")),
                column(12, plotOutput("qq_plot"))
              ),
              fluidRow(
                column(12, h4("Prediction Results:")),
                column(12, verbatimTextOutput("prediction_results"))
              )
            )
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  loaded_data <- reactiveVal(NULL)
  selected_variables <- reactiveVal(character())
  
  # Enter Data Tab
  observeEvent(input$loadDataBtn, {
    req(input$file, input$separator)
    
    loaded_data(read.csv(input$file$datapath, sep = input$separator))
    selected_variables(names(loaded_data()))
  })
  
  output$loaded_data_preview <- renderTable({
    req(loaded_data())
    loaded_data()
  })
  
  # Correlation Tab
  observe({
    req(loaded_data())
    
    # Create UI elements for correlation tab
    output$correlation_variable_selector <- renderUI({
      selectInput("correlation_variables", "Select Variables for Correlation Plot",
                  choices = names(loaded_data()), multiple = TRUE)
    })
    
    output$correlation_tab_plot <- renderPlot({
      req(loaded_data(), input$correlation_variables)
      corr <- cor(loaded_data()[, input$correlation_variables])
      corrplot(corr, method = "number")
    })
    
    # Autokorelasi Test in Correlation Tab
    output$autokorelasi_test_correlation <- renderText({
      req(loaded_data(), input$correlation_variables)
      model <- lm(loaded_data()[[input$correlation_variables[1]]] ~ loaded_data()[[input$correlation_variables[2]]])
      dw_test_result <- dwtest(model)
      paste("Uji Autokorelasi (Durbin-Watson) p-value:", format(dw_test_result$p.value, digits = 4))
    })
    
    # Homoskedastisitas Test in Correlation Tab
    output$homoskedastisitas_test_correlation <- renderText({
      req(loaded_data(), input$correlation_variables)
      model <- lm(loaded_data()[[input$correlation_variables[1]]] ~ loaded_data()[[input$correlation_variables[2]]])
      bptest_result <- bptest(model, studentize = TRUE)
      paste("Uji Homoskedastisitas (Breusch-Pagan) p-value:", format(bptest_result$p.value, digits = 4))
    })
    
    # Kolmogorov-Smirnov Test in Correlation Tab
    output$ks_test_correlation <- renderText({
      req(loaded_data(), input$correlation_variables)
      model <- lm(loaded_data()[[input$correlation_variables[1]]] ~ loaded_data()[[input$correlation_variables[2]]])
      ks_test_result <- lillie.test(model$residuals)
      paste("Uji Kolmogorov-Smirnov p-value:", format(ks_test_result$p.value, digits = 4))
    })
  })
  
  # Regression Tab
  observe({
    req(loaded_data())
    
    output$dependent_selector <- renderUI({
      selectInput("dependent", "Variabel Dependen", choices = names(loaded_data()))
    })
    
    output$independent_selector <- renderUI({
      selectInput("independent", "Variabel Independen", choices = selected_variables(), multiple = TRUE)
    })
    
    output$regression_equation <- renderText({
      req(regression_result())
      regression_result()$regression_equation
    })
    
    # Prediction Tab - Update based on selected independent variables
    observe({
      req(loaded_data(), input$dependent, input$independent)
      
      # Create UI elements for prediction tab
      output$prediction_input_selector <- renderUI({
        tagList(
          lapply(input$independent, function(var) {
            if (is.numeric(loaded_data()[[var]])) {
              numericInput(paste0("predictor_", var), paste("Enter value for", var), value = 0)
            } else {
              selectInput(paste0("predictor_", var), paste("Select value for", var), choices = unique(loaded_data()[[var]]))
            }
          })
        )
      })
    })
  })
  
  predict_result <- eventReactive(input$predictBtn, {
    req(loaded_data())
    
    # Extract model and predictor variables
    model <- regression_result()$model
    
    # Create a data frame for prediction
    new_data <- data.frame(
      setNames(as.list(rep(0, length(names(loaded_data())))), names(loaded_data())),
      stringsAsFactors = FALSE
    )
    
    # Set values for selected predictor variables
    for (var in names(new_data)) {
      if (var %in% input$independent) {
        new_data[1, var] <- input[[paste0("predictor_", var)]]
      }
    }
    
    # Make prediction
    prediction <- predict(model, newdata = new_data)
    
    return(list(
      predictor_vars = names(new_data),
      prediction = prediction
    ))
  })
  
  output$prediction_results <- renderPrint({
    req(predict_result())
    paste(
      "Berdasarkan nilai", paste(input$independent, collapse = ", "),
      "maka hasil prediksinya yaitu", predict_result()$prediction
    )
  })
  
  # Regression Result
  regression_result <- eventReactive(input$estimateBtn, {
    req(loaded_data(), input$dependent, input$independent)
    
    formula_str <- paste(input$dependent, "~", paste(input$independent, collapse = "+"))
    formula <- as.formula(formula_str)
    
    model <- lm(formula, data = loaded_data())
    
    summary_data <- summary(model)
    coefficients_data <- summary_data$coefficients
    
    coefficients <- data.frame(
      Variable = rownames(coefficients_data),
      Estimate = coefficients_data[, 1]
    )
    
    regression_equation <- paste("Estimasi persamaan regresi linier berganda adalah 𝑦̂ =", formatC(coefficients_data[1, 1], digits = 4), "+", paste(formatC(coefficients_data[-1, 1], digits = 4), names(coefficients_data[-1, 1]), collapse = " + "))
    
    return(list(
      coefficients = coefficients,
      summary = summary_data,
      model = model,
      regression_equation = regression_equation,
      dw_test_result = dwtest(model),
      bptest_result = bptest(model, studentize = TRUE),
      ks_test_result = lillie.test(model$residuals)
    ))
  })
  
  output$coefficients <- renderPrint({
    req(regression_result())
    regression_result()$coefficients
  })
  
  output$summary <- renderPrint({
    req(regression_result())
    regression_result()$summary
  })
  
  # Autokorelasi test (Durbin-Watson) in Regression Tab
  output$autokorelasi_test_regression <- renderText({
    req(regression_result())
    paste("Uji Autokorelasi (Durbin-Watson) p-value:", format(regression_result()$dw_test_result$p.value, digits = 4))
  })
  
  # Homoskedastisitas test (Breusch-Pagan) in Regression Tab
  output$homoskedastisitas_test_regression <- renderText({
    req(regression_result())
    paste("Uji Homoskedastisitas (Breusch-Pagan) p-value:", format(regression_result()$bptest_result$p.value, digits = 4))
  })
  
  # Kolmogorov-Smirnov test on residuals in Regression Tab
  output$ks_test_regression <- renderText({
    req(regression_result())
    paste("Uji Kolmogorov-Smirnov p-value:", format(regression_result()$ks_test_result$p.value, digits = 4))
  })
  
  # Regression Plot
  output$regression_plot <- renderPlot({
    req(regression_result())
    plot(regression_result()$model)
  })
  
  # Residual Plot
  output$residual_plot <- renderPlot({
    req(regression_result())
    plot(regression_result()$model$residuals, col = "red", pch = 16, cex = 1.5, main = "Residual Plot")
    abline(h = 0, lty = 2)
  }, height = 400)
  
  # QQ Plot
  output$qq_plot <- renderPlot({
    req(regression_result())
    qqnorm(regression_result()$model$residuals, main = "QQ Plot")
    qqline(regression_result()$model$residuals)
  }, height = 400)
  
  # Instructions in Regression Tab
  output$backward_instructions <- renderText({
    req(regression_result())
    "Lakukan langkah backward pada variabel yang tidak signifikan (p-value > 0.05) satu persatu sampai mendapatkan model yang diinginkan."
  })
}

shinyApp(ui, server)
