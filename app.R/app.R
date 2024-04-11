library(flexdashboard)
library(shiny)
library(jmBIG)
library(survival)
library(dplyr)
library(jmBIG)
library(ggplot2)
library(FastJM)
library(JMbayes2)
library(lme4)
library(rstanarm)
library(joineRML)



  ui_page2 <- fluidPage(
    
    titlePanel("jmbayesBig"),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons("data_option_page2", "Select data source:",
                     c("Upload your own dataset" = "upload",
                       "Use default dataset from jmBIG" = "default")),
        conditionalPanel(
          condition = "input.data_option_page2== 'default'",
          sliderInput("pid_page2", "ID", min = min(surv2$id), max = max(surv2$id), value = 50)
        ),
        conditionalPanel(
          condition = "input.data_option_page2 == 'upload'",
          fileInput("lfile_page2", "Upload Longitudinal Data (CSV format)"),
          fileInput("sfile_page2", "Upload Survival Data (CSV format)"), 
          uiOutput("pid_slider_upload_page2")
        ),
        selectInput("idvar_page2", "ID variable", choices =NULL),
        selectInput("timevar_page2", "Visit time", choices = NULL),
        selectInput('obs_time_page2',"Survival Time variable",choices=NULL),
        selectInput("event_page2", "Event status", choices = NULL),
        textInput("longmodel_page2", "Longitudinal data model", value = "y~1+x7"),
        textInput("rdmodel_page2", "Longitudinal data model random effect part", value = "~visit|id"),
        textInput("survmodel_page2", "Survival data model", value = "Surv(time,status)~1+x1"),
        numericInput("nchain_page2", "Number of chain", value = 1),
        numericInput("samplesize_page2", "Loop size", value = 200),
        actionButton("submit_page2", "Submit")
      ),
      mainPanel(
        p('jmbayesBig(): a function for joint model in BIG DATA using JMbayes2 package. For more information about the arguments of the function please look ',a("here.",
                                                                                                                                                                 href = "https://cran.r-project.org/web/packages/jmBIG/index.html")),
        textOutput('selected_var_page2'),
        textOutput('sample_size_page2'),
        verbatimTextOutput('summarylmemodel_page2'),
        textOutput('selectedID'),
        plotOutput('postTajectory'),
        plotOutput('postSurvival')
        #tabPanel(Title='summary',verbatimTextOutput('summarylmemodel_page2')),
        #tabPanel(Title="Plot",plotOutput('summarylmemodel_page21',width='50%'))
        
        
      )
    )
    
  )
  server_page2 <- function(input, output, session) {
    
    
    
    
    defaultsurvdata_page2 <- reactive({
      if (input$data_option_page2 == 'upload') {
        req(input$sfile_page2)
        defaultsurvdata_page2 <- read.csv(input$sfile_page2$datapath)
      } else {
        defaultsurvdata_page2 <- surv2
      }
      return(defaultsurvdata_page2)
    })
    observeEvent(defaultsurvdata_page2(), {
      updateSelectInput(session, "idvar_page2", choices = names(defaultsurvdata_page2()))
      updateSelectInput(session, "timevar_page2", choices = names(defaultsurvdata_page2()))
      updateSelectInput(session, "event_page2", choices = names(defaultsurvdata_page2()))
      updateSelectInput(session, "obs_time_page2", choices = names(defaultsurvdata_page2()))
    })
    
    
    
    defaultlongdata_page2 <- reactive({
      if (input$data_option_page2 == 'upload') {
        req(input$lfile_page2)
        defaultlongdata_page2 <- read.csv(input$lfile_page2$datapath)
      } else {
        defaultlongdata_page2 <- long2
      }
      return(defaultlongdata_page2)
    })
    
    # Render PID slider for uploaded data
    output$pid_slider_upload_page2 <- renderUI({
      req(input$idvar_page2)
      tagList(
        selectInput("pid_page22", "Patient ID", choices=defaultsurvdata_page2()[[input$idvar_page2]],selectize = FALSE,size=5)
      )
    })
    
    output$selected_var_page2 <- reactive({
      
      if (input$data_option_page2 == "default") {
        req(input$pid_page2)
        paste("You have selected patient ID", input$pid_page2)
      } else {
        req(input$pid_page22)
        paste("Uploaded data selected with patient ID",input$pid_page22)
      }
    })
    
    output$selectedID <- reactive({
      
      if (input$data_option_page2 == "default") {
        req(input$pid_page2)
        paste('Prediction of survival probability and longitudinal marker learned from model fitted using jmbayesBig() for the selected patient ID: ',input$pid_page2)
      } else {
        req(input$pid_page22)
        paste('Prediction of survival probability and longitudinal marker learned from model fitted using jmbayesBig() for the selected patient ID: ',input$pid_page22)
      }
    })
    
    
    output$sample_size_page2 <- renderText({
      paste("Loop size for this computation is", input$samplesize_page2)
    })
    
    rv<-reactiveValues(model_page2=NULL)
    
    observeEvent(input$submit_page2,{
      # Load default data
      output$summarylmemodel_page2 <- renderPrint({
        req(input$samplesize_page2)
        req(input$nchain_page2)
        req(input$timevar_page2)
        req(input$idvar_page2)
        
        rdmodel2<-as.formula(input$rdmodel_page2)
        longmodel2<-as.formula(input$longmodel_page2)
        survmodel2<-as.formula(input$survmodel_page2)
        
        fitmodel_page22 <- jmbayesBig(dtlong = defaultlongdata_page2(), dtsurv = defaultsurvdata_page2(),
                                      longm =longmodel2, survm = survmodel2, rd = rdmodel2,
                                      timeVar = input$timevar_page2, nchain = input$nchain_page2,
                                      samplesize = input$samplesize_page2,
                                      id = input$idvar_page2)
        rv$model_page2<-fitmodel_page22
        fitmodel_page22$pseudoMod
        # list(head(defaultlongdata_page2()),head(defaultsurvdata_page2()),
        #      longmodel2,survmodel2,rdmodel2,input$timevar_page2,input$nchain_page2,
        #      input$samplesize_page2,input$idvar_page2)
        
      })
      
    })
    
    observeEvent(input$submit_page2,{
      # Load default data
      output$postTajectory<- renderPlot({
        
        if(input$data_option_page2 == "default"){
          req(input$samplesize_page2)
          req(input$nchain_page2)
          req(input$timevar_page2)
          req(input$idvar_page2)
          req(input$pid_page2)
          
          idvar_page22<-input$idvar_page2
          pid_page22<-input$pid_page2
          ydt<-defaultlongdata_page2()[defaultlongdata_page2()[idvar_page22]==pid_page22,]
          cdt<-defaultsurvdata_page2()[defaultsurvdata_page2()[idvar_page22]==pid_page22,]
          newdata<-ydt[ydt[input$idvar_page2]==pid_page22,]
          P2_long<-predJMbayes(model=rv$model_page2,ids=pid_page22,newdata=newdata,process = 'longitudinal')
          #head(P2_long$p1[[1]]) 
          plot(P2_long$p1[[1]])
        }else{
          req(input$samplesize_page2)
          req(input$nchain_page2)
          req(input$timevar_page2)
          req(input$idvar_page2)
          req(input$pid_page22)
          
          idvar_page22<-input$idvar_page2
          pid_page221<-input$pid_page22
          ydt<-defaultlongdata_page2()[defaultlongdata_page2()[idvar_page22]==pid_page221,]
          cdt<-defaultsurvdata_page2()[defaultsurvdata_page2()[idvar_page22]==pid_page221,]
          #list(head(defaultsurvdata_page2()),cdt,pid_page221)
          #newdata<-full_join(ydt,cdt,by=idvar_page22)
          #head(newdata)
          newdata<-ydt[ydt[input$idvar_page2]==pid_page221,]
          P2_long<-predJMbayes(model=rv$model_page2,ids=pid_page221,newdata=newdata,process = 'longitudinal')
          #head(P2_long$p1[[1]]) 
          plot(P2_long$p1[[1]])
        }
        
      })
      
    })
    
    observeEvent(input$submit_page2,{
      # Load default data
      output$postSurvival<- renderPlot({
        
        if(input$data_option_page2 == "default"){
          req(input$samplesize_page2)
          req(input$nchain_page2)
          req(input$timevar_page2)
          req(input$idvar_page2)
          req(input$pid_page2)
          
          idvar_page22<-input$idvar_page2
          pid_page22<-input$pid_page2
          ydt<-defaultlongdata_page2()[defaultlongdata_page2()[idvar_page22]==pid_page22,]
          cdt<-defaultsurvdata_page2()[defaultsurvdata_page2()[idvar_page22]==pid_page22,idvar_page22]
          #list(dim(ydt),dim(cdt))
          newdata<-ydt
          #newdata
          P2_event<-predJMbayes(model=rv$model_page2,ids=pid_page22,newdata=newdata,process = 'event')
          #P2_long<-predJMbayes(model=rv$model_page2,ids=pid_page22,newdata=newdata,process = 'longitudinal')
          plot(P2_event$p1[[1]])
          #plot(P2_long$p1[[1]])
        }else{
          req(input$samplesize_page2)
          req(input$nchain_page2)
          req(input$timevar_page2)
          req(input$idvar_page2)
          req(input$pid_page22)
          
          idvar_page22<-input$idvar_page2
          pid_page221<-input$pid_page2
          ydt<-defaultlongdata_page2()[defaultlongdata_page2()[idvar_page22]==pid_page221,]
          cdt<-defaultsurvdata_page2()[defaultsurvdata_page2()[idvar_page22]==pid_page221,]
          #list(dim(ydt),dim(cdt))
          newdata<-ydt
          #newdata
          P2_event<-predJMbayes(model=rv$model_page2,ids=pid_page221,newdata=newdata,process = 'event')
          #P2_long<-predJMbayes(model=rv$model_page2,ids=pid_page22,newdata=newdata,process = 'longitudinal')
          #plot(P2_event$p1[[1]])
          plot(P2_event$p1[[1]])
          
        }
      })
      
    })
  }
  shinyApp(ui = ui_page2, server = server_page2)