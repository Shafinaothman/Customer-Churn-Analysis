library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(shinydashboard)

# Load and clean dataset
dt <- read.csv("/Users/yadshauqi/Bank-Customer-Churn/bank_data.csv")
dt_clean <- na.omit(dt)
dt_clean <- distinct(dt_clean)
dt_clean$gender <- as.factor(dt_clean$gender)
dt_clean$country <- as.factor(dt_clean$country)
dt_clean$churn <- as.factor(dt_clean$churn)
bank_data <- dt_clean

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Bank Customer Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Page", tabName = "main", icon = icon("dashboard")),
      menuItem("Customer Profile Explorer", tabName = "explorer", icon = icon("users")),
      menuItem("Churn Analysis", tabName = "churn", icon = icon("chart-bar")),
      menuItem("Data Summary", tabName = "summary", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                valueBox(nrow(bank_data), "Total Customers", icon = icon("users"), color = "aqua"),
                valueBox(sum(bank_data$churn == 1), "Churned Customers", icon = icon("user-times"), color = "red"),
                valueBox(paste0(round(mean(bank_data$churn == 1) * 100, 2), "%"), "Churn Rate", icon = icon("percentage"), color = "orange")
              ),
              fluidRow(
                box(plotlyOutput("customerCountryBar"), width = 6),
                box(plotlyOutput("ageHistogram"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("activeChurnStacked"), width = 6)
              )
      ),
      tabItem(tabName = "explorer",
              fluidPage(
                titlePanel("Customer Profile Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country", "Select Country:", choices = c("All", "France", "Spain", "Germany"), selected = "All"),
                    selectInput("gender", "Select Gender:", choices = c("All", "Male", "Female"), selected = "All"),
                    sliderInput("age", "Age Range:",
                                min = min(bank_data$age), max = max(bank_data$age),
                                value = c(min(bank_data$age), max(bank_data$age))),
                    selectInput("churn", "Churn Status:", choices = c("All", "Churned", "Not Churned"), selected = "All")
                  ),
                  mainPanel(
                    h4("Filtered Summary Statistics"),
                    fluidRow(
                      valueBoxOutput("filteredTotalCustomers"),
                      valueBoxOutput("filteredAvgBalance"),
                      valueBoxOutput("filteredAvgSalary")
                    ),
                    fluidRow(
                      column(6, plotlyOutput("genderPlot")),
                      column(6, plotlyOutput("churnPlot"))
                    ),
                    h4("Filtered Customer Data"),
                    DTOutput("filteredTable")
                  )
                )
              )
      ),
      tabItem(tabName = "churn",
              fluidPage(
                titlePanel("Churn Analysis"),
                fluidRow(
                  box(plotlyOutput("churnByAge"), width = 6),
                  box(plotlyOutput("churnByCountry"), width = 6)
                ),
                fluidRow(
                  box(plotlyOutput("balanceByChurn"), width = 6),
                  box(plotlyOutput("churnByActiveMember"), width = 6)
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidPage(
                titlePanel("Customer Data Summary"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("sum_country", "Country:", choices = c("All", levels(bank_data$country)), selected = "All"),
                    selectInput("sum_gender", "Gender:", choices = c("All", levels(bank_data$gender)), selected = "All"),
                    sliderInput("sum_age", "Age Range:",
                                min = min(bank_data$age), max = max(bank_data$age),
                                value = c(min(bank_data$age), max(bank_data$age))),
                    selectInput("sum_churn", "Churn Status:", choices = c("All", "Churned", "Not Churned"), selected = "All"),
                    selectInput("selected_hist", "Select Distribution to Show:",
                                choices = c("Credit Score", "Balance", "Estimated Salary"),
                                selected = "Credit Score")
                  ),
                  mainPanel(
                    h4("Summary Statistics"),
                    fluidRow(
                      valueBoxOutput("summaryMeanBox"),
                      valueBoxOutput("summaryMedianBox"),
                      valueBoxOutput("summarySdBox")
                    ),
                    h4("Histogram Distribution"),
                    plotlyOutput("selectedHistogram")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$customerCountryBar <- renderPlotly({
    df <- bank_data %>% count(country)
    gg <- ggplot(df, aes(x = country, y = n, fill = country)) +
      geom_bar(stat = "identity") +
      labs(title = "Number Of Customers By Country", x = "Country", y = "Count") +
      theme_minimal()
    ggplotly(gg)
  })
  
  output$ageHistogram <- renderPlotly({
    gg <- ggplot(bank_data, aes(x = age)) +
      geom_histogram(aes(y = ..density..), binwidth = 5, fill = "red", alpha = 0.6) +
      labs(title = "Distribution Of Customer Ages", x = "Age", y = "Density") +
      theme_minimal()
    ggplotly(gg)
  })
  
  output$activeChurnStacked <- renderPlotly({
    df <- bank_data %>%
      count(active_member, churn) %>%
      group_by(active_member) %>%
      mutate(pct = round(n / sum(n) * 100, 2))
    
    gg <- ggplot(df, aes(x = factor(active_member), y = pct, fill = factor(churn))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("orange", "red"), labels = c("Not Churned", "Churned")) +
      labs(title = "Churn Percentage by Active Member Status", x = "Active Member", y = "Percentage", fill = "Churn") +
      theme_minimal()
    ggplotly(gg)
  })
  
  filteredData <- reactive({
    df <- bank_data
    if (input$country != "All") df <- df %>% filter(country == input$country)
    if (input$gender != "All") df <- df %>% filter(gender == input$gender)
    df <- df %>% filter(age >= input$age[1] & age <= input$age[2])
    if (input$churn == "Churned") df <- df %>% filter(churn == 1)
    else if (input$churn == "Not Churned") df <- df %>% filter(churn == 0)
    return(df)
  })
  
  output$filteredTotalCustomers <- renderValueBox({
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Total Filtered Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$filteredAvgBalance <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$balance, na.rm = TRUE), 2),
      subtitle = "Average Balance",
      icon = icon("wallet"),
      color = "green"
    )
  })
  
  output$filteredAvgSalary <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$estimated_salary, na.rm = TRUE), 2),
      subtitle = "Average Estimated Salary",
      icon = icon("money-bill"),
      color = "purple"
    )
  })
  
  output$genderPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = gender, fill = gender)) +
      geom_bar() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$churnPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = churn, fill = churn)) +
      geom_bar() +
      labs(title = "Churn Distribution", x = "Churn Status", y = "Count") +
      scale_x_discrete(labels = c("0" = "Not Churned", "1" = "Churned")) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$filteredTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  output$churnByAge <- renderPlotly({
    p <- ggplot(bank_data, aes(x = age, fill = churn)) +
      geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
      labs(title = "Churn Distribution by Age", x = "Age", y = "Number of Customers") +
      scale_fill_manual(values = c("0" = "skyblue", "1" = "tomato"), labels = c("Not Churned", "Churned")) +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$balanceByChurn <- renderPlotly({
    p <- ggplot(bank_data, aes(x = churn, y = balance, fill = churn)) +
      geom_boxplot() +
      labs(title = "Customer Balance by Churn", x = "Churn Status", y = "Balance") +
      scale_x_discrete(labels = c("0" = "Not Churned", "1" = "Churned")) +
      scale_fill_manual(values = c("0" = "lightgreen", "1" = "salmon")) +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$churnByActiveMember <- renderPlotly({
    churn_table <- table(bank_data$active_member, bank_data$churn)
    churn_percent <- prop.table(churn_table, margin = 1) * 100
    churn_df <- as.data.frame(as.table(churn_percent))
    colnames(churn_df) <- c("ActiveMember", "Churn", "Percent")
    
    p <- ggplot(churn_df, aes(x = ActiveMember, y = Percent, fill = Churn)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Churn by Active Member Status", x = "Active Member", y = "Percentage") +
      scale_fill_manual(values = c("0" = "deepskyblue", "1" = "firebrick"), labels = c("Not Churned", "Churned")) +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$churnByCountry <- renderPlotly({
    churn_country <- bank_data %>%
      group_by(country, churn) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(churn_country, aes(x = country, y = count, fill = churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Churn by Country", x = "Country", y = "Count") +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "tomato"), labels = c("Not Churned", "Churned")) +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  summaryFiltered <- reactive({
    df <- bank_data
    if (input$sum_country != "All") df <- df %>% filter(country == input$sum_country)
    if (input$sum_gender != "All") df <- df %>% filter(gender == input$sum_gender)
    df <- df %>% filter(age >= input$sum_age[1] & age <= input$sum_age[2])
    if (input$sum_churn == "Churned") df <- df %>% filter(churn == 1)
    else if (input$sum_churn == "Not Churned") df <- df %>% filter(churn == 0)
    return(df)
  })
  
  selectedVar <- reactive({
    df <- summaryFiltered()
    switch(input$selected_hist,
           "Credit Score" = df$credit_score,
           "Balance" = df$balance,
           "Estimated Salary" = df$estimated_salary)
  })
  
  output$summaryMeanBox <- renderValueBox({
    valueBox(
      value = round(mean(selectedVar(), na.rm = TRUE), 2),
      subtitle = paste("Mean of", input$selected_hist),
      icon = icon("calculator"),
      color = "aqua"
    )
  })
  
  output$summaryMedianBox <- renderValueBox({
    valueBox(
      value = round(median(selectedVar(), na.rm = TRUE), 2),
      subtitle = paste("Median of", input$selected_hist),
      icon = icon("sort-numeric-up"),
      color = "yellow"
    )
  })
  
  output$summarySdBox <- renderValueBox({
    valueBox(
      value = round(sd(selectedVar(), na.rm = TRUE), 2),
      subtitle = paste("SD of", input$selected_hist),
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$selectedHistogram <- renderPlotly({
    df <- summaryFiltered()
    variable <- switch(input$selected_hist,
                       "Credit Score" = "credit_score",
                       "Balance" = "balance",
                       "Estimated Salary" = "estimated_salary")
    
    fill_color <- switch(input$selected_hist,
                         "Credit Score" = "#66c2a5",
                         "Balance" = "#fc8d62",
                         "Estimated Salary" = "#8da0cb")
    
    p <- ggplot(df, aes_string(x = variable)) +
      geom_histogram(fill = fill_color, bins = 30, alpha = 0.8) +
      labs(title = paste(input$selected_hist, "Distribution"),
           x = input$selected_hist, y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)