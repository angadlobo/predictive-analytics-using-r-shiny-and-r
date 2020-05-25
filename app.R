#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(caTools)
library(ROCR)
library(caret)
library(dashboard)

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    fileInput("file1", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".xlsx")
    ),
    tags$hr(),
    checkboxInput("header", "Header", TRUE)
  ),
  
  dashboardBody(
    
    
    fluidRow(
      
      infoBox("Employees at risk",   textOutput("yes"), icon = icon("thumbs-down"), fill = TRUE,color = "red",width = 6)
      
    ),
    
    
    
    
    
    fluidRow(
      
      box(title = "Graph", status = "primary", plotOutput("contents"),width = 6,height = 595),
      
      box( title = "Case Analyses Details", status = "primary", height = 
             "595",width = "6",solidHeader = T, 
           column(width = 12,
                  dataTableOutput("content"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
           )
      )
      
    )
    
    
    
    
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  eu_chats_react <- reactive({
    
  })
  
  output$contents <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    empdata=read.csv(inFile$datapath)
    #View(empdata)
    
    # #EDA
    # plot_str(empdata)
    # library(tidyverse)
    # library(funModeling)
    # library(Hmisc)
    # basic_eda <- function(empdata)
    # {
    #   glimpse(empdata)
    #   df_status(empdata)
    #   freq(empdata)
    #   profiling_num(empdata)
    #   plot_num(empdata)
    #   describe(empdata)
    # }
    # basic_eda(empdata)
    
    # #VARIABLE SELECTION
    # fit=lm(Attrition~.,data = empdata)
    # formula(fit)
    # print(summary(fit))
    # cat("STEP-WISE SELECTION METHOD\n")
    # fitstart=lm(Attrition ~ . , data = empdata)
    # print(summary(fitstart))
    # step(fitstart,direction = "both", scope = formula(fit))
    
    #SPLITTING DATA
    split1 =  sample(1:nrow(empdata),as.integer(0.7*nrow(empdata)))
    traindata=empdata[split1,]
    testdata=empdata[-split1,]
    
    
    #BALANCING TRAINING DATA
    library(ROSE)
    data(traindata)
    data.rose <- ROSE(Attrition ~ ., data = traindata, seed = 1)$data
    table(data.rose$Attrition)
    tab2<-data.frame(table(data.rose$Attrition))
    tb3<-toString(tab2[2,2])
    print(tb3)
    
    #MODEL
    model=glm(Attrition ~ Age+OverTime+MaritalStatus+JobInvolvement+EnvironmentSatisfaction+JobSatisfaction+NumCompaniesWorked+DistanceFromHome+YearsInCurrentRole+YearsSinceLastPromotion,data=traindata,family="binomial")                 
    #print(summary(model))
    
    #PREDICTION
    result=predict(model,testdata)
    #print(head(result))
    
    #CONFUSION MATRIX
    conf_matrix=table(Actual_Value=testdata$Attrition, Predict_Value=result>0.5)
    #  print(conf_matrix)
    
    #AcCCURACY
    acc=sum(diag(conf_matrix))/sum(conf_matrix)
    miscl= 1-acc
    cat("---------------------------------------------------\n")
    cat("Accuracy before Sampling =",acc,"\n")
    cat("The Probability of missclassification of the model =",miscl,"\n")
    
    # ROSE.holdout <- ROSE.eval(Attrition ~ ., data = traindata, learner = glm, method.assess = "holdout", seed = 1)
    # ROSE.holdout
    
    
    output$yes<-renderText({
      print(tb3)
      
    })
    
    #GRAPH
    result1=predict(model,traindata,type = "response")
    library(reshape)
    a=melt(head(empdata$Attrition,10))
    b=melt(head(result1,10))
    df=data.frame(a,b)
    colnames(df)=c("Actual","Predicted")
    #  print(df)
    ROCRPred=prediction(result1,traindata$Attrition)
    ROCRPref=performance(ROCRPred,"tpr","fpr")
    plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
    
    
    
    
  })
  
  
  
  output$content<- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })
  
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

