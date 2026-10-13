library(shiny)
library(bslib)
library(DT)

# UI Definition
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "zephyr"),
  title = "Expense & Budget Tracker",
  
  sidebar = sidebar(
    title = "Controls",
    fileInput("file", "Upload input.csv", accept = ".csv"),
    sliderInput("head_n", "Number of First Rows to View:", min = 1, max = 12, value = 3, step = 1),
    helpText("Upload a CSV containing 'Expense' and 'Budget' columns.")
  ),
  
  # Value cards for key metrics
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Average Expense",
      value = textOutput("avg_expense"),
      showcase = bsicons::bs_icon("calculator"),
      theme = "primary"
    ),
    value_box(
      title = "Highest Expense",
      value = textOutput("max_expense"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "danger"
    ),
    value_box(
      title = "Total Months",
      value = textOutput("month_count"),
      showcase = bsicons::bs_icon("calendar-month"),
      theme = "info"
    )
  ),
  
  hr(),
  
  # Main Content Tabs
  navset_card_tab(
    nav_panel(
      title = "First Rows Preview",
      tableOutput("first_three")
    ),
    nav_panel(
      title = "Full Combined Data",
      DTOutput("combined_table")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data reader (uses uploaded file or falls back to 'input.csv')
  data_loaded <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath)
    } else if (file.exists("input.csv")) {
      read.csv("input.csv")
    } else {
      # Fallback mock data if input.csv is missing
      data.frame(
        Expense = c(1200, 1500, 1100, 1800, 1300, 1600),
        Budget = c(1400, 1400, 1400, 1500, 1500, 1500)
      )
    }
  })
  
  # Extract numeric expense vector
  expense_vec <- reactive({
    as.numeric(data_loaded()$Expense)
  })
  
  # Metrics
  output$avg_expense <- renderText({
    exp <- expense_vec()
    if (length(exp) == 0) return("N/A")
    paste0("$", round(mean(exp, na.rm = TRUE), 2))
  })
  
  output$max_expense <- renderText({
    exp <- expense_vec()
    if (length(exp) == 0) return("N/A")
    paste0("$", max(exp, na.rm = TRUE))
  })
  
  output$month_count <- renderText({
    length(expense_vec())
  })
  
  # Display first N rows subset
  output$first_three <- renderTable({
    df <- data_loaded()
    head(df, input$head_n)
  })
  
  # Display combined dataframe with explicit month indexing
  output$combined_table <- renderDT({
    df <- data_loaded()
    df$Month <- seq_len(nrow(df))
    df <- df[, c("Month", setdiff(names(df), "Month"))]
    datatable(df, options = list(pageLength = 5, dom = 'tip'))
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)