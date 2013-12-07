library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  my.stocks <- c()
  my.file.names <- c()  
  my.stocks <- c()
  
  
  output$selectStocks <- renderPrint({
    if (!is.na(input)) {      
      my.file.names <<- input$stocks
      processStockFileData()
    }
  })
  

  processStockFileData <- function() {

    i <- 1
    for (stock in my.file.names) {
      cat(i, ") ", stock, "\n", sep="")
      i <- i+1
    }
    if (i>6) {
      cat("***Error: Maximum of 5 stocks allowed !!")
      return
    }

    if (i>1) {
      cat("Selected Stocks: ", "\n")  
    }
    
    calcDayReturn <- function(stockData) {
      numRec <- nrow(stockData)
      stockData$Day.Return[1] <- 0
      for (i in 2:numRec) {
        stockData$Day.Return[i] <- 
          (stockData$Close.Price[i] - stockData$Close.Price[i-1])*100/stockData$Close.Price[i-1]
      }
      return(stockData)
    }
    
    file.path <- "/home/user/work/Insofe/Mini-Project-2/Stocks/"
    
    check.corel <- FALSE
    dim.len <- length(my.file.names)
    if (dim.len > 1) {
      check.corel <- TRUE
#       matrix.size <- dim.len*dim.len
#       returns.matrix <- matrix(1:matrix.size, nrow=dim.len)
    }

    file.count <- 1
    
    
    for (file.name in my.file.names) {
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
      #Create correlation matrix
      if (file.count == 1) {
        returns.matrix <- matrix(stock.data$Day.Return)
      } else {
        returns.matrix <- cbind(returns.matrix, stock.data$Day.Return)
      }
      #Add data to my.stocks list  
      my.stocks[file.count] <<- list(stock.data)
      file.count <- file.count+1
    }
    
    #Check correlation
    if (check.corel) {
      dimnames(returns.matrix) <- list(NULL, my.file.names)
      cor.matrix <- cor(returns.matrix)
      large.corel <- length(which(cor.matrix > 0.3))
      if (large.corel>5) {
        cat("\nNot a good choice...\n")
        hi.corel <- which(cor.matrix > 0.3, arr.ind=TRUE) 
        len <- length(hi.corel[,1])
        i <- 1
        for (i in 1:len) {
          cor.row <- hi.corel[i,1]
          cor.col <- hi.corel[i,2]
          if (cor.row < cor.col) {
            cat(my.file.names[cor.row], "has hi-correlation with", my.file.names[cor.col], "\n")
          } 
        }
      } else {
        cat("Good job in selecting stocks...")
      }
    }
    
  }

})
