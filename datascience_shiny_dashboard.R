
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(datasets)
library(ggplot2)
library(RColorBrewer)

drivers <- as.data.frame(Seatbelts)
drivers <- drivers %>% 
  rename(
    seatbeltLaw = law,
    distanceTravelled = kms
  )

ui <- fluidPage(
  
  titlePanel("UKDriverDeaths Data Scientist Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h5("Katrina Greene", style = "color:darkblue"),
      p("This project aims to serve data scientists and non-technical stakeholders who want to work with the UKDriverDeaths dataset 
      without having to do low-level R coding themselves. The end user is able to select the 
      training and test data split with a reactive slider; they can then view the resulting data summaries, graphs, and regression models
        fitted to the training data subset selected and the remaining test data subset. Instead of providing only a static presentation
        of data analysis on the UKDriverDeaths dataset, this web app provides an interactive and reactive user experience for a non-technical
        audience that wants to work with this dataset without having to code."),
      setBackgroundColor("grey"),
      h4("Select Training and Test Data Split", style = "color:darkblue"),
      sliderInput("obs",
                  "Number of observations:",
                  min = 0,
                  max = nrow(drivers),
                  value = 170)
    ),
    
    mainPanel(
      setBackgroundColor(
        color = c("white", "lightblue"),
        gradient = "linear",
        direction = "bottom"
      ),
      tabsetPanel(type = "tab",
                  tabPanel("Data Access Login", 
                           h4("User Login"),
                           textInput("name", "Enter Your Name"),
                           passwordInput("pass", "Enter Your Password"),
                           textOutput("text_output"),
                           actionLink("Link", "Forgot Password?")),
                  tabPanel("Summary", 
                           h5("Data Dictionary:"),
                           actionLink("Link", "UKDriverDeaths"),
                           br(),
                           a("https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/UKDriverDeaths.html"),
                           h5("Data Summary:"),
                           verbatimTextOutput("summary"),
                           dataTableOutput("dynamic")),
                  tabPanel("Box Plot", 
                           selectInput("var_box", "Variable", choices = names(drivers)),
                           plotOutput("plot3")),
                  tabPanel("Scatter Plot", 
                           selectInput("DVvar1", "Variable 1", choices = names(drivers)),
                           selectInput("DVvar2", "Variable 2", choices = names(drivers)),
                           plotOutput("plot2")),
                  tabPanel("Simple Linear Regression",
                           selectInput("X", "Predictor Variable", choices = names(drivers)),
                           selectInput("Y", "Response Variable", choices = names(drivers)),
                           fluidRow(
                             column(
                               width = 6,
                               h4("Training Data: "),
                               verbatimTextOutput("simple_model_summary"),
                             ),
                             column(
                               width = 6,
                               h4("Test Data: "),
                               verbatimTextOutput("simple_model_summary_test"),
                             ),
                           ),
                           h5("Training Model Diagnostics:"),
                           plotOutput("diagnostics")
                           ),
                  tabPanel("Multiple Linear Regression",
                           selectInput("X1", "Predictor Variable 1", choices = names(drivers)),
                           selectInput("X2", "Predictor Variable 2", choices = names(drivers)),
                           selectInput("X3", "Predictor Variable 3", choices = names(drivers)),
                           selectInput("Y1", "Response Variable", choices = names(drivers)),
                           fluidRow(
                             column(
                               width = 6,
                               h4("Training Data: "),
                               verbatimTextOutput("mult_model_summary"),
                             ),
                             column(
                               width = 6,
                               h4("Test Data: "),
                               verbatimTextOutput("mult_model_summary_test"),
                             ),
                           ),
                           h5("Training Model Diagnostics:"),
                           plotOutput("diagnostics_mult")
                  ),
        )
                  
      )
  )
)

server <- function(input, output) {
  
  output$text_output <- renderText({
    paste("Username: ", input$name)
  })
  
  train <- reactive(({
    drivers[1:input$obs,]
  }))
  
  test <- reactive(({
    drivers[input$obs+1:nrow(drivers),]
  }))
  
  myColors <- brewer.pal(2, "Accent")
  names(myColors) <- levels(as.factor(drivers$law))
  custom_colors <- scale_color_manual(name = "Seatbelt Law", values = myColors)
  
  output$plot2 <- renderPlot({
    ggplot(train(), aes(x = .data[[input$DVvar1]], y = .data[[input$DVvar2]], color = as.factor(law))) +
      geom_point() +
      geom_smooth(method = lm, color = "red", se = FALSE)
  })
  
  output$plot3 <- renderPlot({
    ggplot(train(), aes(x = .data[[input$var_box]])) +
      geom_boxplot(fill = "lightblue") +
      coord_flip()
    
  })
  
  output$summary <- renderPrint({
    summary(train())
  })
  
  output$dynamic <- renderDataTable({
    train()
  })
  
  simple <- reactive({
    as.formula(paste(input$Y, "~", input$X))
  })
  
  
  train_model <- reactive({
    lm(simple(), data = train())
  })
  
  test_model <- reactive({
    lm(simple(), data = test())
  })
  
  output$simple_model_summary <- renderPrint({
    summary(train_model())
  })
  
  output$simple_model_summary_test <- renderPrint({
    summary(test_model())
  })
  
  output$diagnostics <- renderPlot({
    req(train_model())
    par(mfrow = c(2,2))
    plot(train_model())
  })
  
  multiple <- reactive({
    as.formula(paste(input$Y1, "~", input$X1, "+", input$X2, "+", input$X3))
  })
  
  
  mult_model_train <- reactive({
    lm(multiple(), data = train())
  })
  
  mult_model_test <- reactive({
    lm(multiple(), data = test())
  })
  
  output$mult_model_summary <- renderPrint({
    summary(mult_model_train())
  })
  
  output$mult_model_summary_test <- renderPrint({
    summary(mult_model_test())
  })
  
  output$diagnostics_mult <- renderPlot({
    req(mult_model_train())
    par(mfrow = c(2,2))
    plot(mult_model_train())
  })
  
}


shinyApp(ui = ui, server = server) 


