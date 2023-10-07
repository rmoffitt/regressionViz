
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(
  ui = fluidPage(
    column(1), # blank gutter
    column(11,
           tabsetPanel(
             tabPanel("Linear",
                      fluidRow(
                        column(width = 2,
                               tableOutput('myLinearTable')
                        ),
                        column(width = 10,
                               fluidRow(
                                 column(width = 3,
                                        sliderInput(inputId = "M",
                                                    label = "slope",animate = animationOptions(interval = 333),
                                                    min = -.8, max = 1.2, value = -.1,step = 0.05)
                                 ),
                                 column(width = 3,
                                        sliderInput(inputId = "B",
                                                    label = "intercept",animate = animationOptions(interval = 333),
                                                    min = -3, max = 5, value = -2,step = 0.05)
                                 )
                               ),
                               fluidRow(
                                 column(width = 4,
                                        plotOutput('myLinearPlot')
                                 ),
                                 column(width = 4,
                                        plotOutput('myLinearErrorPlot')
                                 )
                                 ,
                                 column(width = 3,
                                        tableOutput('myLinearErrorTable')
                                 )
                               ),
                               fluidRow(
                                 withMathJax(),
                                 uiOutput('formulaLinear')
                               )
                        )
                      )
             ),
             tabPanel("Logistic",
                      fluidRow(
                        column(width = 2,
                               tableOutput('myLogisticTable')
                        ),
                        column(width = 10,
                               fluidRow(
                                 column(width = 3,
                                        sliderInput(inputId = "m",
                                                    label = "weight",animate = animationOptions(interval = 333),
                                                    min = -0.5, max = 4.5, value = 1,step = 0.1)
                                 ),
                                 column(width = 3,
                                        sliderInput(inputId = "b",
                                                    label = "offset",animate = animationOptions(interval = 333),
                                                    min = -3, max = 5, value = 0,step = 0.1)
                                 )
                               ),
                               fluidRow(
                                 column(width = 4,
                                        plotOutput('myLogisticPlot')
                                 ),
                                 column(width = 4,
                                        plotOutput('myLogisticErrorPlot')
                                 )
                                 ,
                                 column(width = 3,
                                        tableOutput('myLogisticErrorTable')
                                 )
                               ),
                               fluidRow(
                                 withMathJax(),
                                 uiOutput('formulaLogistic')
                               )
                        )
                      )
             ),
             tabPanel("Neural",
                      fluidRow(
                        column(width = 3,
                               tableOutput('myNeuralTable')
                        ),
                        column(width = 9,
                               fluidRow(
                                 column(width = 3,
                                        fluidRow(
                                          sliderInput(inputId = "m1",animate = animationOptions(interval = 333),
                                                      label = "Feature 1 weight",
                                                      min = -0.5, max = 4.5, value = 1,step = 0.1)
                                        ),
                                        fluidRow(
                                          sliderInput(inputId = "m2",animate = animationOptions(interval = 333),
                                                      label = "Feature 2 weight",
                                                      min = -0.5, max = 4.5, value = 1,step = 0.1)
                                        ),
                                        fluidRow(
                                          sliderInput(inputId = "b1",animate = animationOptions(interval = 333),
                                                      label = "offset",
                                                      min = -3, max = 5, value = 0,step = 0.1)
                                        )
                                 ),
                                 column(width = 9,
                                        plotOutput('myNeuralPlot')
                                 )
                               ),
                               fluidRow(
                                 column(width = 3,
                                        tableOutput('myNeuralErrorTable')),
                                 column(width = 3,
                                        plotOutput('myNeuralErrorPlot1')),
                                 column(width = 3,
                                        plotOutput('myNeuralErrorPlot2')),
                                 column(width = 3,
                                        plotOutput('myNeuralErrorPlot3'))
                               ),
                               fluidRow(
                                 withMathJax(),
                                 uiOutput('formulaNeural')
                               )
                        )
                      )
             )
           )
    )
  )
)
