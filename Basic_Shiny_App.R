library(shiny)
library(VIM) #Missing Data Visualizations

options(shiny.maxRequestSize = 15*1024^2)

shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
       theme = "cerulean",  # <--- To use a theme, uncomment this
      "CADRApp",
      tabPanel("Navbar 1",
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 helpText("Default Max. file size is 5mb"),
                 tags$hr(),
                 h5(helpText("Select the Read Table parameters below")),
                 checkboxInput(inputId = "header", label = "Header", value = TRUE),
                 checkboxInput(inputId = "stringsAsFactors", label = "Strings as factors", TRUE),
                 br(),
                 radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=' '), selected = ','),
                 #textInput("txt", "Text input:", "general"),
                 sliderInput("slider", "NA Treshoold Input:", 1, 100, 30),
                 #tags$h5("Deafult actionButton:"),
                 #actionButton("action", "Search"),
                 
                 tags$h5("actionButton with CSS class:"),
                 actionButton("action2", "Action button", class = "btn-primary")
               ),
               mainPanel(
                 uiOutput("tb"))
    #              tabsetPanel(
    #                tabPanel("Missing Values",
    #                         # h4("Table"),
    #                        
    #                         # h4("Verbatim text output"),
    #                         # verbatimTextOutput("txtout"),
    #                         h1("Missing Values"),
    #                         h2("By Variable"),
    #                         tableOutput("table"),
    #                         h2("Graphical Exploration"),
    #                         plotOutput("plot")
    #                ),
    #                tabPanel("Tab 2", "This panel is intentionally left blank"),
    #                tabPanel("Tab 3", "This panel is intentionally left blank")
    #              )
    #            )
       ),
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
       tabPanel("Navbar 3", "This panel is intentionally left blank")
     )
  ),
  server = function(input, output) {
    
    #This reactive function will take the inputs from UI.R and use them for read.table to read the data from the file
    #file$datapath -> gives the path of the file
    data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
        read.table(file=file1$datapath, sep=input$sep, header=input$header, stringsAsFactors= input$stringsAsFactors)
    })

    #this reactive output contains the summary of the dataset and display the summary in a table format
    output$filedf <- renderTable({
      if(is.null(data())){return ()}
      input$file
    })

    output$sum <- renderTable({
      if(is.null(data())){return ()}
      summary(data())
    })

    #This reactive output contains the dataset and display the dataset in a table format
    output$datatable <- renderTable({
      if(is.null(data())){return ()}
      data()
    })
    
    output$tb <- renderUI({
      if(is.null(data()))
        h5("Please Upload the Dataset")
      else
        tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("datatable")),tabPanel("Summary", tableOutput("sum")))
      })

    # output$txtout <- renderText({
    #   paste(input$txt, input$slider, format(input$date), sep = ", ")
    # })
    # 
    # na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
    # na_count <- data.frame(na_count)
    # na_count <- na_count[order(-na_count),,drop = FALSE]
    # na_count <- na_count[na_count>0,,drop = F]
    # 
    # output$table <- renderTable(na_count, striped = FALSE, hover = FALSE, bordered = FALSE,
    #                             spacing = c("s", "xs", "m", "l"), width = "auto", align = NULL,
    #                             rownames = TRUE, colnames = TRUE, digits = NULL, na = "NA",
    #                             env = parent.frame(), quoted = FALSE, outputArgs = list())
    # output$plot <- renderPlot(aggr(subset(data, select= c(rownames(na_count))), numbers = TRUE, prop = c(TRUE, FALSE), varheight = F, sortVars = T, cex.lab = 1.2, cex.axis = 1)) 
  }
)
