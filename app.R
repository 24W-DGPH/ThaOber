# Loading necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)  # For interactive plots

# Importing the dataset
cleaned_data <- read.csv("cleaned_data.csv")

# Categorize age into groups
cleaned_data <- cleaned_data %>%
  mutate(age_group = cut(age, 
                         breaks = c(0, 30, 40, 50, 60, 70, Inf), 
                         labels = c("<30", "30-40", "40-50", "50-60", "60-70", "70+")))

# Check the dataset
head(cleaned_data)

# Define the UI
ui <- fluidPage(
  titlePanel("Heart Disease Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select the visualization
      selectInput("plot_type", "Select a visualization:",
                  choices = c(
                    "Heart Disease Distribution", 
                    "Blood Pressure vs Heart Disease",
                    "Density Plot: Age by Heart Disease",
                    "Density Plot: Stress and Sleep by Heart Disease",
                    "Comparison Between Heart Disease and Diabetes",  # New comparison plot
                    "Heart Disease by Age Group",  # Added this new plot
                    "BMI vs Blood Pressure",  # Added Scatterplot
                    "Cholesterol Distribution by Heart Disease Status"  # Added Cholesterol plot
                  ),
                  selected = "Heart Disease Distribution")  # Default selection
    ),
    
    mainPanel(
      # Output for the selected plot
      plotlyOutput("selected_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Render the selected plot based on the input from the dropdown
  output$selected_plot <- renderPlotly({
    
    if (input$plot_type == "Heart Disease Distribution") {
      # Heart Disease Distribution (Bar plot)
      p <- ggplot(cleaned_data, aes(x = factor(heart_disease), fill = factor(heart_disease))) +
        geom_bar() +
        labs(title = "Heart Disease Distribution",
             x = "Heart Disease (0 = No, 1 = Yes)", y = "Count", fill = "Heart Disease") +
        scale_fill_manual(values = c("#D32F2F", "#388E3C")) +  # Changed colors to red and green
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Heart Disease by Age Group") {
      # Heart Disease Distribution by Age Group (Bar Plot)
      p <- ggplot(cleaned_data, aes(x = age_group, fill = factor(heart_disease))) +
        geom_bar(position = "dodge") +
        labs(title = "Heart Disease Distribution by Age Group",
             x = "Age Group", y = "Count", fill = "Heart Disease (0 = No, 1 = Yes)") +
        scale_fill_manual(values = c("#D32F2F", "#388E3C")) +  # Red and green for heart disease status
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Comparison Between Heart Disease and Diabetes") {
      # Create a comparison plot for Heart Disease and Diabetes
      p <- ggplot(cleaned_data, aes(x = factor(heart_disease), fill = factor(diabetes))) +
        geom_bar(position = "dodge") +
        labs(title = "Comparison Between Heart Disease and Diabetes",
             x = "Heart Disease (0 = No, 1 = Yes)", y = "Count", fill = "Diabetes (0 = No, 1 = Yes)") +
        scale_fill_manual(values = c("#FF5722", "#4CAF50")) +  # Red and Green for Diabetes
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Blood Pressure vs Heart Disease") {
      # Blood Pressure vs Heart Disease (Boxplot)
      p <- ggplot(cleaned_data, aes(x = factor(heart_disease), y = blood_pressure, fill = factor(heart_disease))) +
        geom_boxplot() +
        labs(title = "Blood Pressure vs Heart Disease",
             x = "Heart Disease (0 = No, 1 = Yes)", y = "Blood Pressure", fill = "Heart Disease") +
        scale_fill_manual(values = c("#FF5722", "#4CAF50")) +  # Using distinct colors
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Density Plot: Age by Heart Disease") {
      # Density Plot for Age by Heart Disease Status
      p <- ggplot(cleaned_data, aes(x = age, fill = factor(heart_disease))) +
        geom_density(alpha = 0.6) +
        labs(title = "Density Plot: Age by Heart Disease Status",
             x = "Age", y = "Density", fill = "Heart Disease") +
        scale_fill_manual(values = c("#D32F2F", "#388E3C")) +  # Red for disease, Green for healthy
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Density Plot: Stress and Sleep by Heart Disease") {
      # Density Plot: Stress and Sleep Duration by Heart Disease Status
      p <- ggplot(cleaned_data, aes(x = stress, fill = factor(heart_disease))) +
        geom_density(alpha = 0.6) +
        labs(title = "Density Plot: Stress by Heart Disease",
             x = "Stress Level", y = "Density", fill = "Heart Disease") +
        scale_fill_manual(values = c("#FF5722", "#4CAF50")) +  # Red for disease, Green for healthy
        theme_minimal() +
        theme(legend.position = "top")
      
      # Add sleep duration to the same plot
      p <- p + 
        geom_density(aes(x = sleep), color = "#1976D2", fill = "#1976D2", alpha = 0.3) +
        labs(title = "Density Plot: Stress and Sleep Duration by Heart Disease Status") +
        scale_fill_manual(values = c("#FF5722", "#388E3C", "#1976D2")) +  # Add blue for sleep
        scale_color_manual(values = c("#FF5722", "#388E3C", "#1976D2"))
      
    } else if (input$plot_type == "BMI vs Blood Pressure") {
      # Scatterplot for BMI vs Blood Pressure, colored by Heart Disease
      p <- ggplot(cleaned_data, aes(x = bmi, y = blood_pressure, color = factor(heart_disease))) +
        geom_point(alpha = 0.6) +
        labs(title = "BMI vs Blood Pressure by Heart Disease",
             x = "BMI", y = "Blood Pressure", color = "Heart Disease (0 = No, 1 = Yes)") +
        scale_color_manual(values = c("#D32F2F", "#388E3C")) +  # Red for disease, Green for healthy
        theme_minimal() +
        theme(legend.position = "top")
      
    } else if (input$plot_type == "Cholesterol Distribution by Heart Disease Status") {
      # Cholesterol Distribution by Heart Disease Status (Violinplot)
      p <- ggplot(cleaned_data, aes(x = factor(heart_disease), y = cholesterol, fill = factor(heart_disease))) +
        geom_violin(trim = FALSE) +
        labs(title = "Cholesterol Distribution by Heart Disease Status",
             x = "Heart Disease (0 = No, 1 = Yes)", y = "Cholesterol Level", fill = "Heart Disease") +
        scale_fill_manual(values = c("#FFEB3B", "#F48FB1")) +  # Yellow for No, Pink for Yes
        theme_minimal() +
        theme(legend.position = "top")
      
    } 
    
    # Convert ggplot to interactive plotly plot
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)




