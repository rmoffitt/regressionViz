
helloWorld <- function() {
  print('Hello, world!')
}

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
  fnames <- paste('x',1:features,sep = ".")
  if(labels>0){
    names(df) <- c(fnames,paste('label',1:labels,sep = "."))
  } else {
    names(df) <- fnames
  }
  snames <- paste('sample',1:samples,sep = ".")
  row.names(df) <- snames
  

  return(df)
  
}


