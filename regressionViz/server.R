library(shiny)

shinyServer(function(input, output) {
    
    # ============================================================== #
    #                                                                #
    #      Linear Regression                                         #
    #                                                                #
    # ============================================================== #
    # original data generation
    linearData = reactive({
        x <- makeData(features = 2, samples = 30,
                      translate = TRUE, skew = TRUE,
                      rho = 0.8)
        return(x)
    })
    
    output$formulaLinear <- renderUI({
        withMathJax(
            h2(paste0('prediction = ',round(tan(input$M),digits=2),' * (feature.1 - ',input$B,')'))
        )
    })
    
    # merged with regresasion line
    myLinearData = reactive({
        myCurve <- linearData()
        myCurve$prediction <- tan(input$M)*(myCurve$feature.1 - input$B)
        myCurve$ymin <- apply(myCurve,1,function(x){min(x['feature.2'],x['prediction'])})
        myCurve$ymax <- apply(myCurve,1,function(x){max(x['feature.2'],x['prediction'])})
        return(myCurve)
    })
    
    output$myLinearTable = renderTable({
        tmp <- myLinearData()[1:20,c(1,2,4)]
        return(tmp)
    })
    
    # scatterplot of data and regression
    output$myLinearPlot = renderPlot({
        ggplot(data = myLinearData()) +
            geom_point(aes(x = feature.1,
                           y = feature.2)) + 
            geom_smooth(aes(x = feature.1,
                            y = feature.2),
                        method = 'lm',se = FALSE,color = 'green', lty = 3) +
            geom_line(aes( x = feature.1,
                           y = prediction),
                      lty = 6, color = 'blue',size=1.5) +
            coord_cartesian(#clip = 'off',
                            xlim = c(-5, 5) ,
                            ylim = c(-5, 5)) +
            geom_errorbar(aes( x = feature.1, ymin = ymin,ymax = ymax),
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
                                  sse = sum((myLinearData()$prediction - myLinearData()$feature.2)^2) )
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
            theme_classic(base_size = 25)
        
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
        x <- makeData(features = 1, samples = 50,
                      labels = 1)
        return(x)
    })
    
    output$formulaLogistic <- renderUI({
        withMathJax(
            h2(paste0('likelihood of disease = 1 / ( 1 + exp(',
                      round(-1*input$m,digits=2),
                      ' * feature.1 + ',
                      round(-1*input$b*input$m,digits=2),
                      '))'))
        )
    })
    
    output$myLogisticTable = renderTable({
        tmp <- myLogisticData()[1:20,c(1,2,3)]
        tmp$label.1 <- factor(as.character(tmp$label.1),levels = c("1","0"),labels = c("yes","no"))
        return(tmp)
    })
    
    # merged with regresasion line
    myLogisticData = reactive({
        myCurve <- logisticData()
        myCurve$prediction <- boot::inv.logit(input$m*(myCurve$feature.1 - input$b))
        myCurve$ymin <- apply(myCurve,1,function(x){min(x['label.1'],x['prediction'])})
        myCurve$ymax <- apply(myCurve,1,function(x){max(x['label.1'],x['prediction'])})
        return(myCurve)
    })
    
    # scatterplot of data and regression
    output$myLogisticPlot = renderPlot({
        ggplot(data = myLogisticData()) +
            geom_point(aes(x = feature.1,
                           y = label.1)) + 
            geom_line(aes( x = feature.1,
                           y = prediction),
                      lty = 6, color = 'blue',size=1.5) +
            coord_cartesian(#clip = 'off',
                            xlim = c(-6, 6) ,
                            ylim = c(0, 1)) +
            geom_errorbar(aes( x = feature.1, ymin = ymin,ymax = ymax),
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
            theme_classic(base_size = 25)
        
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
        x <- makeData(features = 2, samples = 50,
                      labels = 1)
        return(x)
    })
    
    output$formulaNeural <- renderUI({
        withMathJax(
            h2(paste0('likelihood of disease = 1 / (1 + exp(',
                      round(-1*input$m1,digits=2),
                      ' * feature.1 + ',
                      round(-1*input$m2,digits=2),
                      ' * feature.2 + ',
                      round(-1*input$b1*input$m,digits=2),
                      '))'))
        )
    })
    
    # merged with regresasion line
    myNeuralData = reactive({
        myCurve <- neuralData()
        myCurve$score <- input$m1*myCurve$feature.1 + 
            input$m2*myCurve$feature.2 + 
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
        ggplot(data = myNeuralData()) +
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
            theme_classic(base_size = 25)
        
    })
    output$myNeuralErrorPlot2 = renderPlot({
        ggplot()+
            geom_point(data = neuralError(),
                       aes(x=m1, y=b1, color=entropy),size = 5) + 
            geom_point(data = subset(neuralError(),entropy == min(entropy)),
                       aes(x=m1, y=b1),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue",guide=FALSE)+
            theme_classic(base_size = 25)
        
    })
    output$myNeuralErrorPlot3 = renderPlot({
        ggplot()+
            geom_point(data = neuralError(),
                       aes(x=b1, y=m2, color=entropy),size = 5) + 
            geom_point(data = subset(neuralError(),entropy == min(entropy)),
                       aes(x=b1, y=m2),color = 'red')+
            scale_color_gradient(low = "yellow", high = "blue",guide=FALSE)+
            theme_classic(base_size = 25)
        
    })
    output$myNeuralErrorTable = renderTable({
        head(neuralError()[order(neuralError()$entropy),],n = 10L)
    })
})


makeData <- function(features = 2,samples = 10,labels = 1,
                     rho = NULL, translate = FALSE, skew = FALSE) {
    m <- rnorm(n = samples*(labels + features))*3
    m <- matrix(data = m, nrow = samples)
    
    if(!is.null(rho)){
        # impart correlation
        L <- matrix(rep(rho,features*features),nrow=features)
        diag(L) <- 1
        Lc <- chol(L)
        m[,1:features]<-m[,1:features]%*%Lc
    }
    if(skew){
        for(i in 1:features){
            m[,i] <- m[,i] * runif(1,min = 0.5,max = 1.5)
        }
    }
    if(translate){
        for(i in 1:features){
            m[,i] <- m[,i] + runif(1,min = -1.5,max = 1.5)
        }
    }
    
    # turn into data frame
    df <- as.data.frame(x = m)
    
    # make labels
    if(labels>0){
        for(i in 1:labels){
            bias = runif(1,min = -2,max = 2)
            df[,i+features] <- bias
            for(j in 1:features){
                slope = runif(1,min = 0.5,max = 2)
                df[,i+features] <- df[,i+features] + df[,j]*slope
            }
            df[,i+features] <- 
                boot::inv.logit(df[,i+features])
            df[,i+features] <- 
                as.numeric(df[,i+features] > runif(samples))
        }
    }
    
    # make good names for the data frame
    fnames <- paste('feature',1:features,sep = ".")
    if(labels>0){
        names(df) <- c(fnames,paste('label',1:labels,sep = "."))
    } else {
        names(df) <- fnames
    }
    snames <- paste('sample',1:samples,sep = ".")
    row.names(df) <- snames
    return(df)
}
