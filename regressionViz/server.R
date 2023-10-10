library(shiny)

shinyServer(function(input, output) {
    
    # ============================================================== #
    #                                                                #
    #      Linear Regression                                         #
    #                                                                #
    # ============================================================== #
    # original data generation
    linearData = reactive({
        x <- makeData(fnames = c('x1','y1'), samples = 30,
                      translate = TRUE, skew = TRUE,
                      rho = 0.8)
        return(x)
    })
    
    output$formulaLinear <- renderUI({
        withMathJax(
            h2(paste0('$$prediction = ',round(tan(input$M),digits=2),' (x1 ',
            if(input$B>=0){'- '}else{''},
            input$B,')$$'))
        )
    })
    
    # merged with regresasion line
    myLinearData = reactive({
        myCurve <- linearData()
        myCurve$prediction <- tan(input$M)*(myCurve$x1 - input$B)
        myCurve$ymin <- apply(myCurve,1,function(x){min(x['y1'],x['prediction'])})
        myCurve$ymax <- apply(myCurve,1,function(x){max(x['y1'],x['prediction'])})
        return(myCurve)
    })
    
    output$myLinearTable = renderTable({
        tmp <- myLinearData()[1:20,c(1,2,4)]
        return(tmp)
    })
    
    # scatterplot of data and regression
    output$myLinearPlot = renderPlot({
        ggplot(data = myLinearData()) +
            geom_point(aes(x = x1,
                           y = y1)) + 
            geom_smooth(aes(x = x1,
                            y = y1),
                        method = 'lm',se = FALSE,color = 'green', lty = 3) +
            geom_line(aes( x = x1,
                           y = prediction),
                      lty = 6, color = 'blue',size=1.5) +
            coord_cartesian(#clip = 'off',
                            xlim = c(-5, 5) ,
                            ylim = c(-5, 5)) +
            geom_errorbar(aes( x = x1, ymin = ymin,ymax = ymax),
                          lty = 3, color = 'red')+
            theme_classic(base_size = 25)
    })
    # initial and running error function
    linearError = reactiveVal({data.frame(M = numeric(0),
                                          B = numeric(0),
                                          sse = numeric(0) )
    })
    observe({
        input$M
        input$B
        isolate({
            newline <- data.frame(M = input$M,
                                  B = input$B,
                                  sse = sum((myLinearData()$prediction - myLinearData()$y1)^2) )
            linearError(unique(rbind(linearError(),newline)))
        })
    })
    # error landscape and data table
    output$myLinearErrorPlot = renderPlot({
        
        ggplot()+
            geom_point(data = linearError(),
                       aes(x=M, y=B, color=sse),size = 5) + 
            geom_point(data = subset(linearError(),sse == min(sse)),
                       aes(x=M, y=B),color = 'red') +
            scale_color_gradient(low = "yellow", high = "blue")+
            theme_classic(base_size = 25) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        
    })
    output$myLinearErrorTable = renderTable({
        head(linearError()[order(linearError()$sse),],n = 10L)
    })
    
    # ============================================================== #
    #                                                                #
    #      Logistic Regression                                       #
    #                                                                #
    # ============================================================== #
    
    logisticData = reactive({
        x <- makeData(fnames = c('x1'), samples = 50,
                      labels = 1)
        return(x)
    })
    
    output$formulaLogistic <- renderUI({
        withMathJax(
            h2(paste0('$$prediction = \\frac{1}{\\left( 1 + e^\\left(',
                      round(-1*input$m,digits=2),
                      ' \\cdot x1 ',
                      if(input$b<=0){'+ '}else{''},
                      round(-1*input$b*input$m,digits=2),
                      '\\right)\\right)}$$'
                      )
               )
        )
    })
    
    output$myLogisticTable = renderTable({
        tmp <- myLogisticData()[1:20,c(1,2,3)]
        tmp$label.1 <- factor(as.character(tmp$label.1),levels = c("1","0"),labels = c("yes","no"))
        return(tmp)
    })
    
    # merged with regression line
    myLogisticData = reactive({
        myCurve <- logisticData()
        myCurve$prediction <- boot::inv.logit(input$m*(myCurve$x1 - input$b))
        myCurve$ymin <- apply(myCurve,1,function(x){min(x['label.1'],x['prediction'])})
        myCurve$ymax <- apply(myCurve,1,function(x){max(x['label.1'],x['prediction'])})
        return(myCurve)
    })
    
    # scatterplot of data and regression
    output$myLogisticPlot = renderPlot({
        ggplot(data = myLogisticData()) +
            geom_point(aes(x = x1,
                           y = label.1)) + 
            geom_line(aes( x = x1,
                           y = prediction),
                      lty = 6, color = 'blue',size=1.5) +
            coord_cartesian(#clip = 'off',
                            xlim = c(-6, 6) ,
                            ylim = c(0, 1)) +
            geom_errorbar(aes( x = x1, ymin = ymin,ymax = ymax),
                          lty = 3, color = 'red')+
            theme_classic(base_size = 25)
    })
    # initial and running error function
    logisticError = reactiveVal({data.frame(m = numeric(0),
                                            b = numeric(0),
                                            entropy = numeric(0) )
    })
    observe({
        input$m
        input$b
        isolate({
            newline <- data.frame(m = input$m,
                                  b = input$b,
                                  entropy = sum(-1*( log(myLogisticData()$prediction) * myLogisticData()$label.1 + 
                                                         log(1 - myLogisticData()$prediction) * (1-myLogisticData()$label.1))) )
            logisticError(unique(rbind(logisticError(),newline)))
        })
    })
    # error landscape and data table
    output$myLogisticErrorPlot = renderPlot({
        ggplot()+
            geom_point(data = logisticError(),
                       aes(x=m, y=b, color=entropy),size = 5) + 
            geom_point(data = subset(logisticError(),entropy == min(entropy)),
                       aes(x=m, y=b),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue")+
            theme_classic(base_size = 25) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        
    })
    output$myLogisticErrorTable = renderTable({
        head(logisticError()[order(logisticError()$entropy),],n = 10L)
    })
    # ============================================================== #
    #                                                                #
    #      Neural Net                                                #
    #                                                                #
    # ============================================================== #
    
    neuralData = reactive({
        x <- makeData(fnames = c('x1','x2'), samples = 50,
                      labels = 1)
        return(x)
    })
    
    output$formulaNeural <- renderUI({
        withMathJax(
          h2(paste0('$$score =',
                    round(-1*input$m1,digits=2),
                    ' \\cdot x1 ',
                    round(-1*input$m2,digits=2),
                    ' \\cdot x2 ',
                    if(input$b<=0){'+ '}else{''},
                    round(-1*input$b1*input$m,digits=2),
                    '~~~~ ',
                    'prediction = \\frac{1}{\\left( 1 + e^\\left(score\\right)\\right)}$$'
                  )
            )
        )
    })
    
    # merged with regresasion line
    myNeuralData = reactive({
        myCurve <- neuralData()
        myCurve$score <- input$m1*myCurve$x1 + 
            input$m2*myCurve$x2 + 
            input$b1
        myCurve$prediction <- boot::inv.logit(myCurve$score)
        myCurve$ymin <- apply(myCurve,1,function(x){min(x['label.1'],x['prediction'])})
        myCurve$ymax <- apply(myCurve,1,function(x){max(x['label.1'],x['prediction'])})
        return(myCurve)
    })
    
    output$myNeuralTable = renderTable({
        tmp <- myNeuralData()[1:25,c(1,2,3,5)]
        tmp$label.1 <- factor(as.character(tmp$label.1),levels = c("1","0"),labels = c("yes","no"))
        return(tmp)
    })    
    
    # scatterplot of data and regression
    output$myNeuralPlot = renderPlot({
        p1 = ggplot(data = myNeuralData()) +
            geom_point(aes(x = score,
                           y = label.1)) + 
            geom_line(aes( x = score,
                           y = prediction),
                      lty = 6, color = 'blue',size=1.5) +
            coord_cartesian(#clip = 'off',
                            xlim = c(-5, 5) ,
                            ylim = c(0, 1)) +
            geom_errorbar(aes( x = score, ymin = ymin,ymax = ymax),
                          lty = 3, color = 'red')+
            theme_classic(base_size = 25)
        print(p1)
    })
    
    # initial and running error function
    neuralError = reactiveVal({data.frame(m1 = numeric(0),
                                          m2 = numeric(0),
                                          b1 = numeric(0),
                                          entropy = numeric(0) )
    })
    observe({
        input$m1
        input$m2
        input$b1
        isolate({
            newline <- data.frame(m1 = input$m1,
                                  m2 = input$m2,
                                  b1 = input$b1,
                                  entropy = sum(-1*( log(myNeuralData()$prediction)*myNeuralData()$label.1 + 
                                                         log(1-myNeuralData()$prediction)*(1-myNeuralData()$label.1))) )
            neuralError(unique(rbind(neuralError(),newline)))
        })
    })
    # error landscape and data table
    output$myNeuralErrorPlot1 = renderPlot({
        ggplot()+
            geom_point(data = neuralError(),
                       aes(x=m1, y=m2, color=entropy),size = 5) + 
            geom_point(data = subset(neuralError(),entropy == min(entropy)),
                       aes(x=m1, y=m2),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue",guide=FALSE)+
            theme_classic(base_size = 25) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        
    })
    output$myNeuralErrorPlot2 = renderPlot({
        ggplot()+
            geom_point(data = neuralError(),
                       aes(x=m1, y=b1, color=entropy),size = 5) + 
            geom_point(data = subset(neuralError(),entropy == min(entropy)),
                       aes(x=m1, y=b1),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue",guide=FALSE)+
            theme_classic(base_size = 25) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        
    })
    output$myNeuralErrorPlot3 = renderPlot({
        ggplot()+
            geom_point(data = neuralError(),
                       aes(x=b1, y=m2, color=entropy),size = 5) + 
            geom_point(data = subset(neuralError(),entropy == min(entropy)),
                       aes(x=b1, y=m2),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue",guide=FALSE)+
            theme_classic(base_size = 25) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        
    })
    output$myNeuralErrorTable = renderTable({
        head(neuralError()[order(neuralError()$entropy),],n = 10L)
    })
})

# ============================================================== #
#      Helper functions                                          #
# ============================================================== #

source('helper_functions.R')

