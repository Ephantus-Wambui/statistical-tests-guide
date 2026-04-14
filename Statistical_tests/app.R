# Load the shiny package
library(shiny)

# Define the User Interface (UI)
ui <- fluidPage(
  
  # App title
  titlePanel("Statistical Test Chooser"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h4("Answer 2 Questions:"),
      
      # Question 1: Study Goal
      selectInput("goal", 
                  "1. What is your study goal?",
                  choices = c(
                    "Compare 1 Group to a Target Value",
                    "Compare 2 Independent Groups",
                    "Compare 2 Paired Groups (Same subjects)",
                    "Compare 3 or more Independent Groups",
                    "Compare 3 or more Paired Groups",
                    "Test for Correlation (Relationship)"
                  )),
      
      # Question 2: Data Shape
      radioButtons("shape", 
                   "2. What is the shape of your data?",
                   choices = c(
                     "Normal / Symmetric (Parametric)" = "normal",
                     "Skewed / Outliers present (Non-Parametric)" = "skewed"
                   )),
      
      # --- NEW: Plot output to show the visual example ---
      plotOutput("shape_example", height = "200px"),
      
      hr(),
      helpText("Tip: If you have massive outliers that make the mean meaningless, always choose 'Skewed / Outliers present'.")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h3("Recommended Statistical Test:"),
      
      # Display the test name in a highly visible box
      wellPanel(
        h2(textOutput("test_name"), style = "color: #2c3e50; font-weight: bold;")
      ),
      
      h4("Why this test?"),
      # Display the explanation
      p(textOutput("explanation"), style = "font-size: 16px;")
    )
  )
)

# Define the Server Logic
server <- function(input, output) {
  
  # --- NEW: Generate the visual examples based on the radio button ---
  output$shape_example <- renderPlot({
    # Set a seed so the random data looks the same every time
    set.seed(123) 
    
    if (input$shape == "normal") {
      # Draw a nice symmetrical bell curve
      data <- rnorm(1000, mean = 50, sd = 10)
      hist(data, breaks = 30, col = "steelblue", border = "white",
           main = "Visual Example: Normal", xlab = "", ylab = "", yaxt = "n",
           cex.main = 1.2)
    } else {
      # Draw a skewed ski-slope distribution
      data <- rlnorm(1000, meanlog = 0, sdlog = 1)
      hist(data, breaks = 30, col = "tomato", border = "white",
           main = "Visual Example: Skewed (Right Tail)", xlab = "", ylab = "", yaxt = "n",
           cex.main = 1.2)
    }
  })
  
  # Reactive function to determine the test and explanation based on inputs
  results <- reactive({
    goal <- input$goal
    shape <- input$shape
    
    # Default empty values
    test <- ""
    desc <- ""
    
    # Logic Tree
    if (goal == "Compare 1 Group to a Target Value") {
      if (shape == "normal") {
        test <- "One-Sample t-test"
        desc <- "Because you are comparing one group to a fixed theoretical target and your data is normal, you can safely test if your sample MEAN equals the target."
      } else {
        test <- "One-Sample Sign Test (or Wilcoxon Signed-Rank)"
        desc <- "Because your data is skewed or has heavy outliers, the mean is biased. The Sign Test safely checks if your sample MEDIAN differs from the target by counting values above and below the threshold."
      }
      
    } else if (goal == "Compare 2 Independent Groups") {
      if (shape == "normal") {
        test <- "Independent Samples t-test"
        desc <- "Because you have two separate, unrelated groups with normally distributed data, you can safely compare their MEANS."
      } else {
        test <- "Mann-Whitney U Test"
        desc <- "Because you have two separate groups but the data is skewed, this non-parametric test compares the ranks (medians) of the groups rather than their means."
      }
      
    } else if (goal == "Compare 2 Paired Groups (Same subjects)") {
      if (shape == "normal") {
        test <- "Paired t-test"
        desc <- "Because you measured the exact same subjects twice (e.g., Before and After) and the differences are normal, you can test the MEAN change."
      } else {
        test <- "Wilcoxon Signed-Rank Test"
        desc <- "Because you measured the same subjects twice but the data is skewed, this test looks at the magnitude and direction of the differences without assuming a bell curve."
      }
      
    } else if (goal == "Compare 3 or more Independent Groups") {
      if (shape == "normal") {
        test <- "One-Way ANOVA"
        desc <- "Because you are comparing three or more distinct groups with normal data, ANOVA allows you to test if any of the group MEANS are significantly different."
      } else {
        test <- "Kruskal-Wallis Test"
        desc <- "This is the non-parametric alternative to ANOVA. It uses ranks to test if the MEDIANS of three or more independent groups differ, protecting you from outliers."
      }
      
    } else if (goal == "Compare 3 or more Paired Groups") {
      if (shape == "normal") {
        test <- "Repeated Measures ANOVA"
        desc <- "Because you measured the same subjects across three or more time points (or conditions) and the data is normal, this test evaluates changes in the MEAN over time."
      } else {
        test <- "Friedman Test"
        desc <- "This is the non-parametric alternative to Repeated Measures ANOVA. It evaluates repeated measurements on the same subjects when the data is skewed."
      }
      
    } else if (goal == "Test for Correlation (Relationship)") {
      if (shape == "normal") {
        test <- "Pearson Correlation"
        desc <- "Because both variables are continuous and normally distributed, Pearson measures the strength of their linear (straight-line) relationship."
      } else {
        test <- "Spearman Correlation"
        desc <- "Because your data is skewed or ordinal (ranked), Spearman measures the relationship using ranks rather than raw values, making it robust against extreme outliers."
      }
    }
    
    return(list(test_name = test, explanation = desc))
  })
  
  # Send the test name to the UI
  output$test_name <- renderText({
    results()$test_name
  })
  
  # Send the explanation to the UI
  output$explanation <- renderText({
    results()$explanation
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
