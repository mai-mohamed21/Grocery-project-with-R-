library("shiny") #gui
library("shinythemes")#gui themes
library("dplyr")#data mainpulation
library("ggplot2")#data visualization
library("gridExtra")#Dashboard
library("arules")#association rules

ui <- fluidPage(
  theme = shinytheme("darkly"), 
  titlePanel("Hotel Reservation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Dataset Path",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      sliderInput("slider", "Choose the number of rows:", 1, 10000, 100),
      sliderInput("clusters", "Number of clusters:", 2, 4, 2),
      sliderInput("min_support", "Minimum Support", min = 0.001, max = 1, value = 0.001, step = 0.001),
      sliderInput("min_confidence", "Minimum Confidence", min = 0.001, max = 1, value = 0.001, step = 0.001)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 tableOutput("contents"),
                 verbatimTextOutput("data_structure")
        ),
        tabPanel("Visualization",
                 plotOutput("pie_chart"),
                 plotOutput("scatter_plot"),
                 plotOutput("bar_plot"),
                 plotOutput("box_plot")
        ),
        tabPanel("Dashboard",
                 plotOutput("Dashboard")
        ),
        tabPanel("Kmeans",
                 tableOutput("kmeans_table"),
                 plotOutput("kmeans_plot")
        ),
        tabPanel("Association Rule",
                 tableOutput("association_output"),
                 plotOutput("item_frequency_plot"),
                 verbatimTextOutput("association_output1")
        )          
      )
    )
  )
)

server <- function(input, output, session) {
  
  clean_data <- reactive({
    req(input$file) 
    dataa <- read.csv(input$file$datapath)
    dataa <- distinct(dataa)
    dataa <- na.omit(dataa)
    return(dataa)
  })
  
  # Reactive expression for common plots
  pie_chart <- reactive({
    finalData <- clean_data()
    x <- table(finalData$paymentType)
    percentage <- sprintf("%.2f%%", round(100 * x / sum(x), 2))
    payment_data <- data.frame(payment_type = names(x), count = as.numeric(x))
    
    ggplot(data = payment_data, aes(x = "", y = count, fill = payment_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Compare Payment Types") +
      theme_minimal() +
      scale_fill_manual(values = c("purple", "pink")) +
      geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Payment Type", reverse = TRUE))
  })
  
  scatter_plot <- reactive({
    finalData <- clean_data()
    new_data <- finalData %>% 
      group_by(age) %>%
      summarise(TotalSpending = sum(total))
    
    ggplot(new_data, aes(x = age, y = TotalSpending)) +
      geom_point(color = "blue") +
      labs(title = "Age vs Total Spending", x = "Age", y = "Total Spending") +
      xlim(22, 65)
  })
  
  bar_plot <- reactive({
    finalData <- clean_data()
    new_data1 <- finalData %>% 
      group_by(city) %>%
      summarise(TotalSpending1 = sum(total))  
    
    ggplot(new_data1, aes(x = reorder(city, -TotalSpending1), y = TotalSpending1)) +
      geom_bar(stat = "identity", fill = "pink") +
      geom_text(aes(label = TotalSpending1), vjust = -0.5, color = "black", size = 3) + 
      labs(title = "Compare Cities Total Spending", x = "City", y = "Total Spending") +
      scale_y_continuous(labels = scales::label_number()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  box_plot <- reactive({
    finalData <- clean_data()
    
    ggplot(finalData, aes(x = "", y = total)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Distribution of Total Spending", x = NULL, y = "Total Spending")})
  
  output$contents <- renderTable({
    finalData <- clean_data()
    head(finalData, input$slider)
  })
  
  output$data_structure <- renderPrint({
    finalData <- clean_data()
    print("The Data is cleaned Successfully")
    print("There are no duplicates")
    print("Data structure is correctly structured")
    print("There are no missing values")
    print("We found a lot of outliers in the count column, but we didn't remove them because every customer is free to choose how many items to buy.")
    str(finalData)
  })
  
  output$pie_chart <- renderPlot({ print(pie_chart()) })
  
  output$scatter_plot <- renderPlot({ print(scatter_plot()) })
  
  output$bar_plot <- renderPlot({ print(bar_plot()) })
  
  output$box_plot <- renderPlot({ print(box_plot()) })
  
  output$Dashboard <- renderPlot({
    grid.arrange(pie_chart(), scatter_plot(), bar_plot(), box_plot(), ncol = 2)
  })
  
  output$kmeans_table <- renderTable({
    finalData <- clean_data()
    Grouping <- finalData %>%
      group_by(customer, age) %>%
      summarise(TotalSpending = sum(total))
    kmeans_result <- kmeans(Grouping[, c("age", "TotalSpending")], centers = input$clusters)
    Grouping$cluster <- kmeans_result$cluster
    return(Grouping)
  })
  
  output$kmeans_plot <- renderPlot({
    finalData <- clean_data()
    Grouping <- finalData %>%
      group_by(customer, age) %>%
      summarise(TotalSpending = sum(total))
    kmeans_result <- kmeans(Grouping[, c("age", "TotalSpending")], centers = input$clusters)
    Grouping$cluster <- kmeans_result$cluster
    plot(Grouping$age, Grouping$TotalSpending, col = Grouping$cluster,
         main = "K-means Clustering", xlab = "Age", ylab = "Total Spending")
    points(kmeans_result$centers, col = 1:input$clusters, pch = 8, cex = 2)
  })
  
  output$association_output <- renderTable({
    finalData <- clean_data()
    x <- finalData$items
    y <- read.transactions(textConnection(x), sep = ",")
    apriori_rules <- apriori(y, parameter = list(supp = input$min_support, conf = input$min_confidence, minlen = 2))
    inspect(apriori_rules)
  })
  
  output$item_frequency_plot <- renderPlot({
    finalData <- clean_data()
    x <- finalData$items
    y <- read.transactions(textConnection(x), sep = ",")
    itemFrequencyPlot(y, topN = 5, type = "absolute")
  })
  
  output$association_output1 <- renderPrint({
    finalData <- clean_data()
    x <- finalData$items
    y <- read.transactions(textConnection(x), sep = ",")
    apriori_rules <- apriori(y, parameter = list(supp = input$min_support, conf = input$min_confidence, minlen = 2))
    inspect(apriori_rules)
  })
}

shinyApp(ui = ui, server = server)