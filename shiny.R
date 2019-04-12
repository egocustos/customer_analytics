library(shiny)
library(tidyverse)
runExample("01_hello")


faithful %>% head

# Shiny built-in examples:

# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer 


library(shinydashboard)


ui <- fluidPage(

  
  # Put them together into a dashboardPage
  dashboardPage(
    dashboardHeader(title = "Simple tabs"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Load data", tabName = "load", icon = icon("dashboard")),
        menuItem("Null Value filter", tabName = "null", badgeColor = "yellow"),
        menuItem("High Correlation filter", tabName = "corr", badgeColor = "yellow"),
        menuItem("Low Variance filter", tabName = "var", badgeColor = "yellow"),
        menuItem("PCA", tabName = "pca", badgeColor = "blue")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "load",
                h2("Load your data")
        ),
        
        tabItem(tabName = "null",
                h2("Missing Value Filter"),
                h5("This filter removes all columns with more missing values than a selected threshold.")
        ),
        tabItem(tabName = "corr",
                h2("Correlation")
        )
      )
    )
  )
)


server <- function(input, output){}

shinyApp(ui=ui,server=server)












