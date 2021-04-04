library(shiny)
library(shiny.router)
library(GGally)
library(ggplot2)
library(ggiraphExtra)
library(caret)
library(dplyr)
library(reshape2)
#library(visreg)

df=read.csv("Concrete_Data_Yeh.csv")



#################################################################
#Page Definition

home_page <- div(
  titlePanel("Compressive Strength of Concrete"),
  p("Welcome!"),
  # Sidebar with a slider input for number of bins
  tags$div(class="NormText"),
  tags$p("This Shiny App will help you explore and predict the strengh of concrete, based on linear regression methods."),
  tags$p("Navigate the app via the header panel above. You can explore univariate methods, multivariate methods, or review a report containing some mathematical background and a quick primer in concrete!"),
)

################################################################
## Univariate UI


single_page <- div(
  titlePanel("Univariate"),
  p("Univariate analysis of concrete compressive strength. Choose how much of the data set you would like use to train the model, and which variable you would like it trained against."),
  p("If you want some hints on what variables are significant impactors, check the box to show the correlation chart."),
  p("The scatterplot and prediction plot will not show up until the Update button is pressed."),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("tSamp","Percentage of dataset for training:",min = 50,max = 80,value = 75),
      
      varSelectInput("variable", "Variable:", df[,1:8], selected = "cement"),
      
      checkboxInput("corr", "Plot Correlation Chart?", value = FALSE),
      
      hr(),
      
      actionButton("go", "Update")
      
      
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      hr(),
      
      conditionalPanel(
        condition = "input.corr == true", 
        plotOutput("corrPlot")
      ), 
      
      
      hr(),
      plotOutput("scatterPlot"),
      hr(),
      plotOutput("predPlot"),
      
      verbatimTextOutput("uvSum", placeholder=TRUE)
      
    )
  )
)



################################################################
## Multivariate UI

multi_page <- div(
  
  titlePanel("Multivariate"),
  p("Multivariate analysis of concrete compressive strength. Choose as many variables as you wish to study their relationship with compressive strength."),
  p("A multi variable scatterplot is shown to illustrate how much variation there is in the data."),
  p("The scatterplot and prediction plot will not show up until the Update button is pressed."),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mtSamp","Percentage of dataset for training:",min = 50,max = 80,value = 75),
      varSelectInput("variables", "Variable:", df[,1:8], multiple = TRUE, selected =c("cement", "water")),
      checkboxInput("mCorr", "Plot Correlation Chart?", value = FALSE),
      hr(),
      actionButton("mGo", "Update")
      ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.mCorr == true", 
        plotOutput("mCorrPlot")
      ), 
      hr(),
      #ggiraphOutput("mVis"),
      plotOutput("mVis"),
      plotOutput("mPredPlot"),
      verbatimTextOutput("mvSum", placeholder=TRUE),
      
      )
    )

)


background_page <- div(
  titlePanel("Background Data"),
  p("Exploratory Analysis and Project Life Cycle"),
  includeHTML("Case_Study_3_Final.html")
)

contact_page <- div(
  titlePanel("Contact"),
  p("Contact Information"),
  includeHTML("links.html")
)

router <- make_router(
  route("/", home_page),
  route("background", background_page),
  route("Univariate", single_page),
  route("Multivariate", multi_page),
  route("contact", contact_page)
)

#################################################################
#UI


ui <- fluidPage(
  theme = "main.css",
  tags$ul(
    tags$li(a(href = route_link("/"), "Welcome!")),
    tags$li(a(href = route_link("Univariate"), "Univariate")),
    tags$li(a(href = route_link("Multivariate"), "Multivariate")),
    tags$li(a(href = route_link("background"), "Background")),
    tags$li(a(href = route_link("contact"), "Contact"))
  ),
  router$ui
)


#################################################################
#Server


server <- function(input, output, session) {
  router$server(input, output, session)
  
  
#################################################################  
## Univariate server logic  
    x1 = eventReactive(input$go, {splitDF=createDataPartition(df[,1], p = input$tSamp/100, list=F, times=1)})
  
    trainDat = reactive({ df[x1(),]})
    testDat= reactive({df[-x1(),]})

    lr = reactive({
      lm(reformulate(as.character(input$variable), "csMPa"), data=trainDat())
    })
    
    output$corrPlot = renderPlot({
        ggcorr(df, c("pairwise", "pearson"), label = TRUE, label_size = 3, layout.exp= 4, hjust = 0.75)
    })
    
    
    output$scatterPlot = renderPlot({
      p=ggplot(trainDat(), aes_string(input$variable)) + geom_point(aes(y=trainDat()[, "csMPa"]), colour = "#002366", size = 3) +
        geom_abline(intercept = lr()[1]$coefficients[1], slope = lr()[1]$coefficients[2], color="red", size=2) +
        ylim(-5,90) 
      p
    })
    
    output$predPlot = renderPlot({
      predStrength = data.frame(predict(lr(), newdata=testDat()))
      names(predStrength)[1] = 'Predicted'
      predStrength$Reference = testDat()[,c('csMPa')]
      predStrength$lwr = predict(lr(), newdata=testDat(), interval = "confidence")[,2]
      predStrength$upr = predict(lr(), newdata=testDat(), interval = "confidence")[,3]
      
      p3 = qplot(Reference, Predicted, data=predStrength) + geom_point(colour = "#002366", size = 3) + 
        geom_errorbar(aes(ymin = lwr,ymax = upr)) + xlim(-5,90) + ylim(-5,90)
      p3
    })
    
    output$uvSum = renderPrint({
      summary(lr())
    })

################################################################# 
## Multivariate server logic    
    
    
    output$mCorrPlot = renderPlot({
      ggcorr(df, c("pairwise", "pearson"), label = TRUE, label_size = 3, layout.exp= 4, hjust = 0.75)
    })
    
    
    x2 = eventReactive(input$mGo, {splitDF=createDataPartition(df[,1], p = input$tSamp/100, list=F, times=1)})
    
    trainDatm = reactive({ df[x2(),]})
    testDatm= reactive({df[-x2(),]})
    
    #inputDat = reactive({testDatm()[,as.character(input$variables)]})
    
    inputDat = reactive({
      if (length(input$variables) == 1){
        temp=data.frame(1:nrow(testDatm()))
        temp[1]=testDatm()[,as.character(input$variables)]
        names(temp)[1] = as.character(input$variables)
        return(temp)
      }

      else{
        return(testDatm()[,as.character(input$variables)])
        }

      })
    
    
    mlr = reactive({
      lm(reformulate(as.character(input$variables), "csMPa"), data=trainDatm() )
    })   
    
    
    output$mPredPlot = renderPlot({
      
      mPredStrength = data.frame(predict(mlr(), inputDat(), level=.95, interval="confidence"))
      names(mPredStrength)[1] = 'Predicted'
      mPredStrength$Reference = testDatm()[,c(9)]
      mPredStrength$mLwr = predict(mlr(), newdata=testDatm(), interval = "confidence")[,2]
      mPredStrength$mUpr = predict(mlr(), newdata=testDatm(), interval = "confidence")[,3]
      
      qplot(Reference, Predicted, data=mPredStrength) + geom_point(colour = "#002366", size = 3) + geom_errorbar(aes(ymin = mLwr,ymax = mUpr))
    })
    
    output$mVis = renderPlot({
      #ggPredict(mlr())
      dfLong= melt(df, id = "csMPa")
      test = filter(dfLong, dfLong$variable== as.character(input$variables) ) 
      ggplot(test, aes(csMPa, value, colour = variable)) + geom_point()
    })
    
        
    output$mvSum = renderPrint({

      summary(mlr())
      })

  }

shinyApp(ui, server)
