library(shiny)
library(shinydashboard)

source("E:/Users/Richard.Vogg/Documents/Customer Analytics/PoC_ML/Helper.R")

#Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "Analysis Project"
)

#Sidebar ------------------------------------------------------------------
sidebar <- dashboardSidebar(
  menuItem(text = "Importing file", tabName = "dashboard",icon = icon("file"),
           menuSubItem(text = "Load data", tabName = "rawdata"),
           menuSubItem(text = "Review data",tabName = "review"),
           menuSubItem(text ="Raw data summary",tabName="rawsumm")
  ),
  menuItem("Basic reduction techniques", tabName = "brt",icon=icon("broom"),
           menuSubItem(text="Manual column remover",tabName='manual'),
           menuSubItem(text="Missing values column filter", tabName = 'mvf'),
           menuSubItem(text='Null variance filter', tabName = 'lvf'),
           menuSubItem(text="High Correlation filter",tabName='hcf')
  ),
  menuItem("Advanced reduction techniques",tabName = 'art',icon=icon("filter"),
           menuSubItem(text="Principal Component Analysis",tabName='pca'),
           menuSubItem(text='Variable Importance',tabName='varimp')
           ),
  menuItem("Other useful functions",tabName='other',icon=icon("key"),
           menuSubItem(text="Missing value imputation",tabName='mvi'),
           menuSubItem(text="Category To Other",tabName='cto'),
           menuSubItem(text="Encoding",tabName='enc'),
           menuSubItem(text="Sampling",tabName='samp'),
           menuSubItem(text="Classification",tabName='ml')
  ),
  menuItem("Exporting",tabName="expo",icon=icon("save"),
           menuSubItem(text="Save data",tabName='save'),
           menuSubItem(text="Download model",tabName='download'))
  
)

#Body ------------------------------------------------------------------
body<- dashboardBody(
    tabItems(
      
      ##########################
      
      tabItem(
        tabName = "rawdata",
        fluidPage(
                box(width = 7,
                    fileInput('datafile', 
                              'Choose Data',
                              multiple = FALSE,
                              accept = c('text/csv',
                                         'text/comma-separated-values,text/plain',
                                         '.csv')),
                    checkboxInput(inputId = "header", label = "Header", value = TRUE),
                    checkboxInput(inputId = "stringsAsFactors", label = "Strings as factors", TRUE),
                    br(),
                    radioButtons(inputId = 'sep', label = 'Separator', 
                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=' '), 
                                 selected = ',')
                ),
                box(title = 'Delete columns', collapsible = T, width = 3, solidHeader = T,
                    uiOutput('whichcolumnsdelete'),
                    actionButton('applydelete', 'Apply Deletion')
                )
              )
      ),
      
      ######################
      tabItem(
        tabName = 'review',
        fluidPage(
          box(width=12,
              h2("Review the data"),
              tableOutput("print_data")
          )
        )
      ),
      
      
      ######################
      
      tabItem(
        tabName = 'rawsumm',
        fluidPage(
          box(width=12,
              h2("Summary"),
              verbatimTextOutput("print_summary")
          )
        )
      ),
      
      ######################
      
      tabItem(
        tabName = 'manual',
        fluidPage(
          box(width=12,
              h2("Remove columns manually")
          ),
          box(width=6,
              uiOutput("checkbox_manual")
          ),
          box(width=6,
              actionButton("remove","Remove selected columns",
                           icon=icon("splotch")))
        )
      ),
      
      ####################
      
      tabItem(
        tabName = 'mvf',
        fluidPage(
          box(width=12,
              h2("Missing Values"),
              p("Review how many missing values each column has. If a column has
                too many missing values it might not contain a lot of information
                and can possibly be removed.")
              ),
          box(width=5,
              plotOutput("mvf_plot"),
              sliderInput("mvf_slider",h3("How many % of missing values do you allow
                          a column to have?"),min=0,max=1,value=0.5),
              actionButton("mvf_button","Apply",icon=icon("splotch"))
              )
        )
      ),
      
      ########################
      
      tabItem(tabName = 'lvf',
          fluidPage(
            box(width=12,
                h2('Variance Filter'),
                p("By using this filter, we remove all variables that only consist of one value
                  (zero variance) and categorical variables that have too many different categories (e.g. indices)")
                ),
            box(width=8,
                tableOutput("lvf_table")
                ),
            box(width=3,
                actionButton("variance","Apply",icon=icon("splotch"))
                )
          )
              
      ),
      
      ########################
      
      tabItem(tabName = 'hcf',
              fluidPage(
                box(width=12,
                    h2('High Correlation Filter'),
                    p("By using this filter, we remove highly correlated features.
                      The algorithm automatically detects which of two correlated features
                      add less value to the model and removes this feature. The user can
                      input a threshold, which is the maximal Correlation allowed between
                      two features.")
                    ),
                box(width=8,
                    p("These are the highest correlations between features that were detected:"),
                    tableOutput("hcf_table")
                ),
                box(width=3,
                    sliderInput("hcf_slider",h3("What is the highest allowed correlation
                                                between two features?"),min=0,max=1,value=0.5),
                    actionButton("hcf_button","Apply",icon=icon("splotch"))
                )
                )
              
    ),
      
      
      ########################
      
      tabItem(
        tabName = 'pca',
        fluidPage(
          box(width=12,
              h2("Principal component analysis"),
              p("This method will try to reduce the number of features while
                maintaining as much information as possible. We will lose the
                column names and get unnamed new features which are combinations
                from the original ones.")
              ),
          box(
            width=5,
            p("Mark the predictor variable"),
            uiOutput("colcheckboxes4"),
            p("This program will run for a few minutes. It ends when the importance
              plot appears on the right side"),
            actionButton("pca_button","PCA",icon=icon("splotch"))
            ),
          box(width=5,
              plotOutput("pcaplot"),
              sliderInput("featurecut2", h3("How many features do you want to keep"),
                          min = 0, max = 200, value = 20),
              actionButton("pca_button2","Remove not important features",icon=icon("splotch"))
          )
        )
        ),
      ########################
      
      tabItem(
        tabName = 'varimp',
        fluidPage(
          box(width=12,
              h2("Variable Importance Ranking"),
              p("Make sure that your variables do not contain missing values.
                To check the amount of missing values, go to Other useful functions ->
                Missing value imputation.")
          ),
          box(
            width=5,
            p("Mark the predictor variable"),
            uiOutput("colcheckboxes3"),
            p("This program will run for a few minutes. It ends when the importance
              plot appears on the right side"),
            actionButton("varimp","Variable importance",icon=icon("splotch"))
          ),
          box(width=5,
              plotOutput("varimpplot"),
              sliderInput("featurecut", h3("How many features do you want to keep"),
                          min = 0, max = 200, value = 20),
              actionButton("varimp2","Remove not important features",icon=icon("splotch"))
          )
        )
      ),
      
      ########################
      
      tabItem(tabName = 'mvi',
        box(width=12,
            h2('Missing Value Imputation'),
            p("Imputation of missing values"),
            tableOutput("miss_summ"),
            actionButton("imputation","Apply",icon=icon("splotch"))
            )
              
      ),
    
      ########################
    
      tabItem(
        tabName='cto',
        fluidPage(
          box(width=12,
              h2("Category to \"other\""),
              p("Check if there are variables with too many categories and low 
                frequencies. Select the variables that you would like to transform
                and select the minimum frequency that you expect you variables to have."),
              tableOutput("cat_summary")
              ),
          box(width=5,
              sliderInput("cat_thres","Frequency threshold",min=0,max=500,value=100),
              actionButton("cat_to_other","Apply",icon=icon("splotch"))
              ),
          infoBoxOutput(width=2,"progressBox")
        )
        ),
      
      
      ########################
      
      tabItem(tabName = 'enc',
        h2('Encoding'),
        fluidPage(
          box(width=10,
              p("Change categorical variables into numerical ones. Methods:
                Binary Encoding"),
              uiOutput("colcheckboxes5"),
              actionButton("binary","Apply",icon=icon("splotch"))
              )
        )
              
      ),
      
      
      ########################
      
      tabItem(tabName = 'samp',
        h2('Sampling'),
        fluidPage(
          box(
            width=12,
            p("Preparation for Machine Learning algorithms, especially in the
          case of unbalanced classes (i.e. one class is much bigger that 
              the other one). Given the response
              variable, upsample/downsample to have equal size of each class.")
          ),
          box(
            width=6,
            uiOutput("colcheckboxes")
          ),
          box(
            width=5,
            tableOutput("samptable")
          ),
          box(
            width=5,
            actionButton("sampling","Apply",icon=icon("splotch"))
          )
        )

        ),
      
      ##########################
      
      tabItem(tabName = 'ml',
        h2('Machine Learning Methods'),
        fluidPage(
          box(width=12,
            p("We will split the dataset into train and test, apply
            the algorithm on the train set and evaluate its performance
            on the test set. We chose the area under the curve (AUC) of the
            ROC curve as an evaluation metric.")),
          box(
            width=5,
            p("Mark the variable that you want the algorithm to learn
              and later predict"),
            uiOutput("colcheckboxes2")
          ),
          box(width=6,
              p("The variables for this algorithm need to be numerical. For conversion 
                see Other useful functions --> Encoding"),
              actionButton("logreg","Logistic regression",icon=icon("splotch")),
              verbatimTextOutput("logregtab"),
              valueBoxOutput("logregacc",width=NULL)
          ),
          box(width=6,
              actionButton("rf","Random Forest",icon=icon("splotch")),
              verbatimTextOutput("rftab"),
              valueBoxOutput("rfacc",width=NULL)
          ),
          box(width=6,
              sliderInput("nn_size",h3("Select the size of the Neural Network"),
                          min=2,max=20,value=6),
              actionButton("nn","Neural Networks",icon=icon("splotch")),
              verbatimTextOutput("nntab"),
              valueBoxOutput("nnacc",width=NULL)
          )
          
        )
      ),
    
      ########################
      
      tabItem(tabName = 'save',
              h2('Save dataset'),
              p("Export your final dataset as a .csv file"),
              fluidPage(
                box(width=10,
                    downloadButton("downloadData", "Download")
                    )
              )
              
      ),
    
      #######################
    
    tabItem(tabName = 'download',
            h2('Download dataset'),
            p("Missing")
            
    )
  )
)
# UI -----------------------------------------------------------------------------
ui <- dashboardPage(header = header,sidebar = sidebar,body = body,skin="yellow")

#Server Function -----------------------------------------------------------------

server <- function(input, output,session) {
  
  data <- reactiveValues(orig_data=NULL,
                         imp_cols=NULL)
  
  #Load and review data
  observeEvent(input$datafile,{
    dat <- read.csv(input$datafile$datapath,sep=input$sep,header=input$header,
                    stringsAsFactors = input$stringsAsFactors)
    data$orig_data <- dat
  })
  
  output$print_data <- renderTable({
    data$orig_data
  })
  
  output$print_summary <- renderPrint({
    summary(data$orig_data)
  })
  
  #Remove columns manually
  
  output$checkbox_manual <- renderUI({
    checkboxGroupInput(inputId="cols7",
                 label="Column names:",
                 choices=colnames(data$orig_data))
  })
  
  observeEvent(input$remove,{
    dat <- data$orig_data
    dat[,input$cols7] <- NULL
    data$orig_data <- dat
  })
  
  
  #Remove columns with too many missing values
  
  output$mvf_plot <- renderPlot({
    dat <- data$orig_data
    mv_perc <- lapply(dat,function(x) sum(is.na(x))/nrow(dat)) %>% {data.frame(col=unlist(.))}
    ggplot(mv_perc,aes(x=col))+geom_histogram()
  })
  
  observeEvent(input$mvf_button, {
    data$orig_data <- NULLremover(data$orig_data,input$mvf_slider)
  })
  
  #Remove zero variance and too many categories
  
  output$lvf_table <- renderTable({
    sd <- sapply(data$orig_data,sd,na.rm=T )
    ndc <- sapply(data$orig_data,nlevels)
    data.frame(colnames=colnames(data$orig_data),standard_dev=sd,
               different_categories=ndc)
  })
  
  
  observeEvent(input$variance, {
    dat <- SDremover(data$orig_data,0)
    dat <- select_if(dat,function(x) is.numeric(x) | nlevels(x)<=500)
    data$orig_data <- dat
  })
  
  #Remove highly correlated variables
  output$hcf_table <- renderTable({
    dat <- data$orig_data
    dat_num <- dat %>% select_if(is.numeric)
    corr_matrix <- data.frame(cor(dat_num,use='complete.obs'))
    corr_matrix$id <- rownames(corr_matrix)
    corr_matrix <- corr_matrix %>% gather(key="key1",value="corr",-id) %>%
      filter(corr<1) %>% arrange(desc(abs(corr)))
    data.frame(corr_matrix[c(1,3,5,7,9),])
  })
  
  observeEvent(input$hcf_button, {
    dat <- data$orig_data
    dat_num <- dat %>% select_if(is.numeric)
    hc <- findCorrelation(cor(dat_num,use='complete.obs'),cutoff=input$hcf_slider)
    remove <- colnames(dat_num)[hc]
    col <- colnames(dat_num)
    remove <- col[hc]
    dat <- dat[,setdiff(colnames(dat),remove)]
    data$orig_data <- dat
  })
  
  
  #Principal Component Analysis
  
  output$colcheckboxes4 <- renderUI({
    radioButtons(inputId="cols4",
                 label="Column names:",
                 choices=colnames(data$orig_data))
  })
  
  observeEvent(input$pca_button, {
    dat <- data$orig_data
    name <- input$cols4
    train <- dat[,setdiff(colnames(dat),name)]
    pca <- prcomp(train,scale.=T)
    train_labels <- dat[,name]
    #Make a Plot
    std_dev <- pca$sdev
    pr_var <- std_dev^2
    prop_varex <- data.frame(id=1:length(pr_var),prop=pr_var/sum(pr_var))
    print(prop_varex)
    
    output$pcaplot <- renderPlot({
      ggplot(prop_varex,aes(x=id,y=prop))+geom_line()+geom_point()
    })
    
    data$orig_data <- cbind(train_labels,pca$x)
  })
  
  observeEvent(input$pca_button2,{
    dat <- data$orig_data
    dat <- dat[,1:(input$featurecut2+1)]
    cat("Done")
    data$orig_data <- dat
  })
  
  
  #Variable Importance
  
  output$colcheckboxes3 <- renderUI({
    radioButtons(inputId="cols3",
                 label="Column names:",
                 choices=colnames(data$orig_data), selected=2)
  })
  
  observeEvent(input$varimp, {
    fitControl <- trainControl(method = "boot",
                               verboseIter = TRUE, #Output while running
                               classProbs= TRUE, #needed for ROC
                               summaryFunction = twoClassSummary ) #needed for ROC
    rf_grid <- expand.grid(mtry=round(sqrt(ncol(data$orig_data))),
                           splitrule="gini",
                           min.node.size=100)
    dat <- data$orig_data
    name <- input$cols3
    train <- dat[,setdiff(colnames(dat),name)]
    train_labels <- dat[,name]
    set.seed(1)
    rf <- train(x=train, 
                y=factor(make.names(train_labels)),
                method="ranger", metric="ROC",num.trees=500,
                trControl=fitControl,tuneGrid=rf_grid,importance = 'impurity')
    imp <- varImp(rf)
    cat("Done")
    
    data$imp_cols <- imp$importance %>% 
      mutate(col=rownames(.)) %>% 
      arrange(desc(Overall))
    
    output$varimpplot <- renderPlot({
      ggplot(imp)
    })
    
  })
  
  observeEvent(input$varimp2,{
    dat <- data$orig_data
    ind <- which(colnames(dat) %in% 
                   c(data$imp_cols[1:input$featurecut,c("col")],input$cols3))
    dat <- dat[,ind]
    cat("Done")
    data$orig_data <- dat
  })
  
  
  
  
  #Missing Values Imputation
  output$miss_summ <- renderTable({
    datatype <- sapply(data$orig_data,class)
    missing_values <- sapply(data$orig_data,function(x) sum(is.na(x)))
    data.frame(colnames(data$orig_data),datatype,
               missing_values)
  })
  
  observeEvent(input$imputation, {
    dat <- data$orig_data %>%
      mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.)))
    output$progressBox <- renderInfoBox({
      infoBox(
        "Done", icon = icon("list"),
        color = "green"
      )})
    data$orig_data <- dat
  })
  
  
  
  #Category to Other
  output$cat_summary <- renderTable({
    number_categories <- sapply(data$orig_data,num_cat)
    min_freq <- sapply(data$orig_data,min_freq)
    mean_freq <- sapply(data$orig_data,mean_freq)
    max_freq <- sapply(data$orig_data,max_freq)
    data.frame(colnames(data$orig_data),number_categories,
               min_freq,mean_freq,max_freq)
  })
  
  
  observeEvent(input$cat_to_other, {
    output$progressBox <- renderInfoBox({
      infoBox(
        "Please wait", icon = icon("list"),
        color = "red"
      )})
    dat <- lapply(data$orig_data,function(x) categoryToOther(x,input$cat_thres)) %>%
      data.frame()
    output$progressBox <- renderInfoBox({
      infoBox(
        "Done", icon = icon("list"),
        color = "green"
      )})
    data$orig_data <- dat
  })
  
  
  #Encoding
  
  output$colcheckboxes5 <- renderUI({
    radioButtons(inputId="cols5",
                 label="Column names:",
                 choices=colnames(data$orig_data))
  })
  
  observeEvent(input$binary, {
    output$box_bin <- renderInfoBox({
      infoBox("Please wait", icon = icon("list"),color = "red"
      )})
    name <- input$cols5
    dat <- data$orig_data
    dat <- cbind(predictor=dat[,name],lapply(dat[,setdiff(colnames(dat),name)],
                  function(x) x <- binaryEncoding(x)) %>% data.frame())
    data$orig_data <- dat
  })
  
  #Sampling
  
  output$colcheckboxes <- renderUI({
      radioButtons(inputId="cols",
                         label="Column names:",
                         choices=colnames(data$orig_data))
  })
  
  observeEvent(input$cols, {
    output$samptable <- renderTable({table(data$orig_data[,input$cols])})
  })
  
  observeEvent(input$sampling, {
    
    dat <- downSample(x = data$orig_data, 
                      y = factor(data$orig_data[,input$cols]))
    data$orig_data <- dat[,1:(ncol(dat)-1)]
  })
  
  #Machine Learning
  
  output$colcheckboxes2 <- renderUI({
    radioButtons(inputId="cols2",
                 label="Column names:",
                 choices=colnames(data$orig_data))
  })
  
  
  
  observeEvent(input$logreg, {
    fitControl <- trainControl(method = "CV", #Cross-validation
                               number = 3, #3-fold
                               verboseIter = TRUE, #Output while running
                               classProbs= TRUE, #needed for ROC
                               summaryFunction = twoClassSummary ) #needed for ROC
    dat <- data$orig_data
    set.seed(64)
    ind <- sample(1:nrow(dat),0.8*(nrow(dat)))
    name <- input$cols2
    train <- dat[ind,setdiff(colnames(dat),name)]
    test <- dat[setdiff(1:nrow(dat),ind),setdiff(colnames(dat),name)]
    train_labels <- dat[ind,name]
    test_labels <- dat[setdiff(1:nrow(dat),ind),name]
    lr <- train(x=train, 
                y=factor(make.names(train_labels)), 
                method="glm", metric="ROC",
                trControl=fitControl)#,tuneGrid=lr_grid
    lr_pred <- predict(lr,test,type="prob")
    print(colnames(lr_pred))
    t <- table(lr_pred[,2]>0.5,factor(make.names(test_labels)))
    output$logregtab <- renderPrint({
      t
    })
    output$logregacc <- renderValueBox({
      valueBox(paste0(100*round(accuracy(t),3),"%"),"Accuracy:",
               icon=icon("thumbs-up"),color="yellow")
    })
  }
  )
  
  observeEvent(input$rf, {
    fitControl <- trainControl(method = "CV", #Cross-validation
                               number = 3, #3-fold
                               verboseIter = TRUE, #Output while running
                               classProbs= TRUE, #needed for ROC
                               summaryFunction = twoClassSummary ) #needed for ROC
    dat <- data$orig_data
    set.seed(64)
    ind <- sample(1:nrow(dat),0.8*(nrow(dat)))
    name <- input$cols2
    train <- dat[ind,setdiff(colnames(dat),name)]
    test <- dat[setdiff(1:nrow(dat),ind),setdiff(colnames(dat),name)]
    train_labels <- dat[ind,name]
    test_labels <- dat[setdiff(1:nrow(dat),ind),name]
    rf_grid <- expand.grid(mtry=ceiling(ncol(train)/2),
                           splitrule="gini",min.node.size=100)
    rf <- train(x=train, 
                y=factor(make.names(train_labels)),
                method="ranger", metric="ROC",num.trees=500,
                trControl=fitControl,tuneGrid=rf_grid,importance = 'impurity')#,tuneGrid=lr_grid
    rf_pred <- predict(rf,test,type="prob")
    t <- table(rf_pred[,2]>0.5,factor(make.names(test_labels)))
    output$rftab <- renderPrint({
      t
    })
    output$rfacc <- renderValueBox({
      valueBox(paste0(100*round(accuracy(t),3),"%"),"Accuracy:",
               icon=icon("thumbs-up"),color="yellow")
    })
  }
  )
  
  observeEvent(input$nn, {
    fitControl <- trainControl(method = "CV", #Cross-validation
                               number = 3, #3-fold
                               verboseIter = TRUE, #Output while running
                               classProbs= TRUE, #needed for ROC
                               summaryFunction = twoClassSummary ) #needed for ROC
    dat <- data$orig_data
    set.seed(64)
    ind <- sample(1:nrow(dat),0.8*(nrow(dat)))
    name <- input$cols2
    train <- dat[ind,setdiff(colnames(dat),name)]
    test <- dat[setdiff(1:nrow(dat),ind),setdiff(colnames(dat),name)]
    train_labels <- dat[ind,name]
    test_labels <- dat[setdiff(1:nrow(dat),ind),name]
    nn_grid <- data.frame(size=input$nn_size)
    nn <- train(x=train, 
                y=factor(make.names(train_labels)),
                method="mlp", metric="ROC",
                trControl=fitControl,tuneGrid=nn_grid,
                preProcess = c("center","scale"))
    nn_pred <- predict(nn,test,type="prob")
    t <- table(nn_pred[,2]>0.5,factor(make.names(test_labels)))
    output$nntab <- renderPrint({t})
    output$nnacc <- renderValueBox({
      valueBox(paste0(100*round(accuracy(t),3),"%"),"Accuracy:",
               icon=icon("thumbs-up"),color="yellow")
    })
  }
  )
  
  #Save file
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "download.csv"
    },
    content = function(file) {
      write.csv(data$orig_data, file, row.names = FALSE)
    }
  )
}

#Run Shiny Dashboard
shinyApp(ui = ui, server = server)
