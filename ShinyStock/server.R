library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  formulaText <- reactive({
    paste("", input$stock)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  stock.data <- NULL
  
  output$descStats <- renderPrint ({
    stock.data <<- processStockData()
    my.stock.stats <- data.frame(matrix(nrow=1, ncol=6,dimnames=list(NULL,
                                c("Name", "Mean", "Median", "Std.Dev", "CI.Low", "CI.Hi"))))
    my.stock.stats$Name <- input$stock
    my.stock.stats$Mean <- mean(stock.data$Day.Return)
    my.stock.stats$Median <- median(stock.data$Day.Return)
    my.stock.stats$Std.Dev <- sd(stock.data$Day.Return)
    my.stock.stats$CI.Low <- qnorm(0.05, mean=my.stock.stats$Mean, sd=my.stock.stats$Std.Dev)
    my.stock.stats$CI.Hi <- qnorm(0.95, mean=my.stock.stats$Mean, sd=my.stock.stats$Std.Dev)
    my.stock.stats
  })
  
  my.colors.light <- c("lightpink", "lightsalmon", "lightskyblue3",  "lightsteelblue4", "lightyellow", "lightcoral", "lightgoldenrod1" )
  my.colors.dark <- c("violetred", "slategray", "royalblue2", "maroon", "deeppink3", "darkorchid3", "darkseagreen3" )
  
  processStockData <- function() {
    calcDayReturn <- function(stockData) {
      numRec <- nrow(stockData)
      stockData$Day.Return[1] <- 0
      for (i in 2:numRec) {
        stockData$Day.Return[i] <- 
          (stockData$Close.Price[i] - stockData$Close.Price[i-1])*100/stockData$Close.Price[i-1]
      }
      return (stockData)
    }
    
    #Get full path
    file.name <- paste("/home/user/work/Insofe/Mini-Project-2/Stocks/", input$stock, sep="")
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
    
    return(stock.data)
  }
  
  stock.data <- NULL
  
  output$returnsPlot <- renderPlot({
    #my.colors.dark <- colors()[grep(pattern="darks",x=colors())]
    my.color <- my.colors.dark[sample(1:length(my.colors.dark),1)]
    plot(c(1:length(stock.data$Day.Return)), stock.data$Day.Return, data=stock.data$Day.Return, geom="line", 
           main=input$stock, xlab="Day", col=my.color, type="l",  lwd=2, xaxt="n",
           ylab="Returns")
  })
  
  stock.freq.breaks <- c(-8,-4,-1,0,3,5,10)
  output$freqPlot <- renderPlot ({
    hist(stock.data$Day.Return,col=my.colors.light, breaks=stock.freq.breaks, labels=TRUE, freq=TRUE,
         main=paste("Histogram of", input$stock))    
    curve(dnorm(x, mean=mean(stock.data$Day.Return), sd=sd(stock.data$Day.Return)), 
          add=TRUE, col="maroon", lwd=2)
    axis(1, stock.freq.breaks, NULL)
  })
  
})
