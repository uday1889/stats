library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Stock Portfolio Guru"),
  
  sidebarPanel(
    
    selectInput("profile", "Investment Profile:",
                 c("Distribute Equally" = "equal",
                   "Highest Returns" = "hi.ret",
                   "Lowest Risks" = "low.risk",
                   "Optimal" = "optimal")),
    br(),
    
    checkboxGroupInput("stocks", "Stock:",
                       list("Asian Paints"="Asian Paints.csv", 
                            "Bharati Airtel"="Bharati Airtel.csv", 
                            "BHEL"="BHEL.csv", 
                            "Cipla"="Cipla.csv", 
                            "Coal INDIA Ltd"="Coal INDIA Ltd.csv", 
                            "DLF"="DLF.csv", 
                            "Dr. Reddy's"="Dr. Reddy's.csv", 
                            "GAIL"="GAIL.csv", 
                            "HCL TECH"="HCL TECH.csv", 
                            "HDFC Bank"="HDFC Bank.csv", 
                            "Hero Motor Corp Ltd"="Hero Motor Corp Ltd.csv", 
                            "ICICI Bank"="ICICI Bank.csv", 
                            "ITC"="ITC.csv", 
                            "Jindal Steel"="Jindal Steel.csv", 
                            "LT"="LT.csv", 
                            #                                 "Mahindra & Mahindra"="Mahindra & Mahindra.csv", 
                            #                                 "Maruti Suzuki"="Maruti Suzuki.csv", 
                            #                                 "ONGC"="ONGC.csv", 
                            #                                 "Punjab National Bank"="Punjab National Bank.csv", 
                            #                                 "Ranbaxy"="Ranbaxy.csv", 
                            "SBI"="SBI.csv", 
                            "Tata Motors"="Tata Motors.csv", 
                            "Tata Steel"="Tata Steel.csv", 
                            "TCS"="TCS.csv", 
                            "Ultra Tech Cements"="Ultra Tech Cements.csv")),
    
    br(),
    submitButton("Show Results") 
    
  ),
  
  
  
  mainPanel(
    verbatimTextOutput("checkStocks"),
    plotOutput("returnsPlot",height=200),
    verbatimTextOutput("returnsDetails")
  )
))
