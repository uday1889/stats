library(shiny)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  my.file.names <- reactive(getFileNames())  
  my.stocks <- reactive(processStockFileData())
  
  output$checkStocks <- renderPrint({
    
    if (is.na(input)) {
      return(NULL)
    }
    
      #List stock names
      i <- 1
      for (stock in my.file.names()) {
        if (i == 1) {
          cat("Selected Stocks: ", "\n")  
        }
        cat(i, ") ", stock, "\n", sep="")
        i <- i+1
      }
            
      returns.matrix <- NULL
      #Create correlation matrix
      for (stock in my.stocks()) {
        if (i == 1) {
          returns.matrix <- matrix(stock$Day.Return)
        } else {
          returns.matrix <- cbind(returns.matrix, stock$Day.Return)
        }
      }

      #Check correlation
      if (length(my.stocks()) >1) {
        dimnames(returns.matrix) <- list(NULL, my.file.names())
        cor.matrix <- cor(returns.matrix)
        large.corel <- length(which(cor.matrix > 0.3))
        if (large.corel>length(my.file.names())) {
          cat("\nNot a good choice...\n")
          hi.corel <- which(cor.matrix > 0.3, arr.ind=TRUE) 
          len <- length(hi.corel[,1])
          i <- 1
          for (i in 1:len) {
            cor.row <- hi.corel[i,1]
            cor.col <- hi.corel[i,2]
            if (cor.row < cor.col) {
              cat(my.file.names()[cor.row], " has hi-correlation (",
                  cor.matrix[cor.row, cor.col],") with ", 
                  my.file.names()[cor.col], "\n", sep="")
            } 
          }
        }
      }
  })
  
  getFileNames <- function() {
    return (input$stocks)    
  }
  
  processStockFileData <- function() {
    #Calculates the daily returns for specified stock data
    calcDayReturn <- function(stockData) {
      numRec <- nrow(stockData)
      stockData$Day.Return[1] <- 0
      for (i in 2:numRec) {
        stockData$Day.Return[i] <- 
          (stockData$Close.Price[i] - 
             stockData$Close.Price[i-1])*100/stockData$Close.Price[i-1]
      }
      return(stockData)
    }
    
    file.path <- "/home/user/work/Insofe/Mini-Project-2/Stocks/"
    stock.data <- NULL
    file.count <- 1
    stocks <- c()
    for (file.name in my.file.names()) {
      file.name <- paste(file.path, file.name, sep="")
      #Load data from file
      stock.data <- read.csv(file.name)
      #Format date column
      stock.data$Date <- as.Date(stock.data$Date, format="%d-%b-%y") 
      #Sort data based on Date col
      stock.data <- stock.data[order(stock.data$Date, decreasing=FALSE),]
      #Extract training data - 1 year data
      stock.data <- stock.data[1:207,]
      #Add a column to store day returns
      stock.data <- calcDayReturn(stock.data)
      #Add data to my.stocks() list  
      stocks[file.count] <- list(stock.data)
      file.count <- file.count+1
    }
    return(stocks)
  }
  
  #Plot charts
  myColors <- c("slateblue3", "red", "violetred1", "darkgoldenrod", "steelblue3", "darkseagreen3",
                "dodgerblue2", "darksalmon")
  
  ########### INDIVIDUAL PLOTS OF DAILY RETURNS ###########
  output$returnsPlot <- renderPlot ({
    num.img <- length(my.file.names())
    
    if (num.img>0) {
    plotDayReturn <- function(dayReturnVector, chartTitle, lineColor) {
      plot(c(1:length(dayReturnVector)), dayReturnVector, data=dayReturnVector, geom="line", 
           main=chartTitle, xlab="Day", col=c(lineColor), type="l",  
           ylab="Returns")
    }
    
    i <- 1
    par(mfrow=c(1,num.img))
    for (my.stock in my.stocks()) {
      plotDayReturn(my.stock$Day.Return, my.file.names()[i], myColors[i])
      i <- i+1
    }
    }
  })
  ########### INDIVIDUAL PLOTS OF DAILY RETURNS ###########
  
  output$returnsDetails <- renderPrint ({
  })
  
})
