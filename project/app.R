library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)

ui <- fluidPage( includeCSS("www/bootstrap.css"), 
                 navbarPage(h1("THE MOJITO GAME",
                               style = "font-family: 'Neucha', cursive;
                               font-weight: 500; line-height: 1.1; font-size : 60px;
                               color: #28a745;"),
                            
                            tabPanel(
                              h1("Rules of the game", 
                                 style = "font-family: 'Neucha', cursive;
                                 font-weight: 500; line-height: 1.1; 
                                 color: #28a745;"),
                              tags$body("Welcome to our game ! 
                                        Here you will be the manager of a Mojito Stand.

                                        note that you can only go to negative 200$ meaning you cannot loan more money
                                        i also changed marketing to 100$ dollar instead on 250$ 
                                        also each 10 seconds you lose stock ammount*0.1$ and 1$ per server
                                        ")),
                            
                            tabPanel(h1("The Game", 
                                        style = "font-family: 'Neucha', cursive;
                                        font-weight: 500; line-height: 1.1; 
                                        color: #28a745;"), 
                                     
                                     useShinyalert(),
                                     useShinyjs(),
                                     column(10, offset=4, titlePanel("Welcome to the Mojito Game!"),style="padding-bottom: 20px;padding-top: 20px;"),
                                     tags$style(HTML(".tabbable > .nav > li > a {margin-top:300px;}")),
                                     
                                     div(id="startbutton",
                                         
                                         actionBttn(
                                       inputId = "start",
                                       label = "start",
                                       color = "primary",
                                       size = "lg",
                                       style = "unite"),
                                       
                                       style="padding-left: 550px;padding-top: 20px;"
                                       ),
                                     
                                     
                                     hidden( div(  id="game",
                                                   
                                     fluidRow(style="border: 4px double black;",
                                              
                                              column(12,h3(textOutput("Elapsedtime"),style = "padding-bottom: 10px;padding-top: 10px;")),  
                                              column(2,tags$h3("Number of sold Mojitos :",style = "padding-bottom: 10px")),
                                              column(1,h3(textOutput("saled_mojito")))
                                              
                                              
                                     ),
                                     
                                     fluidRow(style="border: 4px double black;",
                                              tags$h2("Cost Parameter",style = "padding-top: 10px;padding-left: 10px;"),
                                              column(12,
                                                     tags$h4("Marketing Level (max 10)"),
                                                     column(2,verbatimTextOutput("marketinglevel")),
                                                     column(1, 
                                                            actionButton(
                                                              inputId = "addmarketing",
                                                              label = icon("plus"),
                                                              class = "btn btn-success" 
                                                            )),
                                                     column(12),
                                                     column(2, tags$h5("Upgrade Marketing for 100$",style = "padding-bottom: 10px"))
                                                     
                                              ),
                                              
                                              column(5,
                                                     tags$h4("Number of Waiters:"),
                                                     column(1,
                                                            actionButton(
                                                              inputId = "lessserver",
                                                              label = icon("minus"),
                                                              class = "btn btn-danger" )),
                                                     column(4,verbatimTextOutput("numberofserver")),
                                                     column(2,
                                                            actionButton(
                                                              inputId = "addserver",
                                                              label = icon("plus"),
                                                              class = "btn btn-success"
                                                            )),
                                                     column(12),
                                                     column(6,tags$h5("Each waiter costs 1$ per 10 seconds",style = "padding-bottom: 10px;")) 
                                              ),
                                              column(2,tags$h4("Total costs per 10 seconds are:"),
                                                     verbatimTextOutput("totalcost")
                                                     ,style="padding-bottom: 10px;"
                                              )
                                              
                                     ),
                                     
                                     fluidRow(style="border: 4px double black;",
                                              tags$h2("Supply Parameter",style = "padding-top: 10px;padding-left: 10px;"),
                                              
                                              column(12,
                                                     column(6,tags$h4("Lemon Stock"),
                                                            column(5,verbatimTextOutput("lemonstock")),
                                                            column(2,
                                                                   actionButton(
                                                                     inputId = "addlemon",
                                                                     label = icon("plus"),
                                                                     class = "btn btn-success")),
                                                            tags$h5("Buy 10 units of lemon for 5$",style="padding-top: 7px;")
                                                     ),
                                                     
                                                     column(6,tags$h4("Mint Stock"),
                                                            column(5,verbatimTextOutput("mintstock")),
                                                            column(2,
                                                                   actionButton(
                                                                     inputId = "addmint",
                                                                     label = icon("plus"),
                                                                     class = "btn btn-success")),
                                                            tags$h5("Buy 20 units of mint for 5$",style="padding-top: 7px;")
                                                            
                                                            
                                                            
                                                     ),
                                                     column(6,tags$h4("Sugar Stock"),
                                                            column(5,verbatimTextOutput("sugarstock")),
                                                            column(2,
                                                                   actionButton(
                                                                     inputId = "addsugar",
                                                                     label = icon("plus"),
                                                                     class = "btn btn-success")),
                                                            tags$h5("Buy 30 units of sugar for 5$",style="padding-top: 7px;")
                                                            
                                                            
                                                     ),
                                                     
                                                     column(6,tags$h4("Rhum Stock"),
                                                            column(5,verbatimTextOutput("rhumstock")),
                                                            column(2,
                                                                   actionButton(
                                                                     inputId = "addrhum",
                                                                     label = icon("plus"),
                                                                     class = "btn btn-success")),
                                                            tags$h5("Buy 10 units of rhum for 10$",style="padding-top: 7px;")
                                                            
                                                     )
                                              ),
                                              
                                              column(4,
                                                     tags$h4("Number of Mojitos that we could make with this stock:",style = "padding-top: 10px;"),
                                                     verbatimTextOutput("mojitonumber"),style = "padding-bottom: 10px;"
                                              )),
                                     
                                     fluidRow(style="border: 4px double black;",
                                              
                                              tags$h2("Price and Ambiance Setting",style = "padding-top: 10px;padding-left: 10px;"),
                                              column(12,
                                                     tags$h4("Price Setting"),
                                                     column(5, 
                                                            column(1,
                                                                   actionButton(
                                                                     inputId = "decreaseprice",
                                                                     label = icon("minus"),
                                                                     class = "btn btn-danger")
                                                                   
                                                            ),
                                                            column(2,
                                                                   
                                                                   verbatimTextOutput("Price")
                                                            ),
                                                            column(1,
                                                                   
                                                                   actionButton(
                                                                     inputId = "increaseprice",
                                                                     label = icon("plus"),
                                                                     class = "btn btn-success")
                                                                   
                                                            ))
                                              ),
                                              column(6,
                                                     tags$h4("Ambiance Setting"),
                                                     selectInput(inputId = "ambiance", width = '50%', label = "ambiance", choices = c("Classic","Electro","House","Jazz")
                                                     )
                                                     ,style = "padding-bottom: 10px;")
                                              
                                              
                                     ),
                                     
                                     fluidRow(style="border: 4px double black;",
                                              column(12,tags$h2("Money Output",style = "padding-top: 10px;"),
                                                     column(4,
                                                            tags$h4("Piggy Bank"),
                                                            verbatimTextOutput("piggy")
                                                            
                                                            
                                                     ), 
                                                     column(12),
                                                     column(4,tags$h4("Gross Profit over time"),
                                                            verbatimTextOutput("profit")
                                                            ,style = "padding-bottom: 10px;")
                                                     
                                              )),
                                     
                                     fluidRow(style="border: 4px double black;",
                                              column(12,  tags$h2("Graphics",style = "padding-top: 10px;padding-bottom: 10px;"), 
                                                     column(5,
                                                            
                                                            radioButtons("type", "Choose a plot type:",
                                                                         choiceValues =  list("Profit", "Number_of_mojito_sold"),
                                                                         choiceNames = list("Profit", "Number of Mojitos sold")),
                                                            div(style="padding-bottom: 10px;"),
                                                            plotOutput("myPlot")
                                                     ),
                                                     
                                                     column(5,tags$h4("Dataframe of what has happened:",style="padding-bottom: 10px;"),
                                                            actionBttn(
                                                              inputId = "refresh",
                                                              label = "refresh",
                                                              color = "primary",
                                                              style = "pill"),
                                                            div(style="padding-bottom: 20px;"),
                                                            dataTableOutput('loc'))
                                                     
                                              )))
                            )
                 
                 
                              )))


server <- function(input, output, session) {
  
  observeEvent(input$start,{
    
    # Define server logic to show elapsed time, update every second ----
    show("game")
    hide("startbutton")
  
  
    # Define server logic to show elapsed time, update every second ----
    
    initialtime <- Sys.time()
    
    
    output$Elapsedtime <- renderText({
      invalidateLater(1000, session)
      paste("The elapsed time", round(difftime(Sys.time(),initialtime,units = "secs")),"seconds")
    })
    
    ## set reactive value
    
    rv <- reactiveValues(lines = data.frame(), n = 0, marketing_level = 1, piggy_bank = 1000, Price = 1, saled_mojito = 0, numberofserver=1,totalcost = 0,lemonstock = 10, mintstock = 30, sugarstock=20, rhumstock=10, mojitonumber = 0, ambiance = 1,x=data.frame())
    
    
    
    ## do something when add marketing is clicked
    
    observeEvent(input$addmarketing, {
      if(rv$marketing_level < 10 ){
        if(rv$piggy_bank >= 100){
          rv$marketing_level <- rv$marketing_level + 1     # if the add button is clicked, increment the value by 1 and update it
          rv$piggy_bank <- rv$piggy_bank - 100
        } else {rv$marketing_level <- rv$marketing_level}}else {rv$marketing_level <- rv$marketing_level}
    })
    
    
    
    
    ##add lemon when click on button
    
    observeEvent(input$addlemon,
                 {
                   
                   if(rv$piggy_bank>=5){
                     rv$piggy_bank <- rv$piggy_bank - 5
                     rv$lemonstock <- rv$lemonstock + 10}
                 })
    
    ##add mint when click on button
    
    observeEvent(input$addmint,
                 {
                   
                   if(rv$piggy_bank>=5){
                     rv$piggy_bank <- rv$piggy_bank - 5
                     rv$mintstock <- rv$mintstock + 20}
                 })
    ##add mint when click on button
    
    observeEvent(input$addsugar,
                 {
                   
                   if(rv$piggy_bank>=5){
                     rv$piggy_bank <- rv$piggy_bank - 5
                     rv$sugarstock <- rv$sugarstock + 30}
                 })
    
    ##add mint when click on button
    
    observeEvent(input$addrhum,
                 {
                   
                   if(rv$piggy_bank>=10){
                     rv$piggy_bank <- rv$piggy_bank - 10
                     rv$rhumstock <- rv$rhumstock + 10}
                 })
    
    # increase or deacrease price button handled.. 
    
    observeEvent(input$increaseprice, if (rv$Price<20)rv$Price <- rv$Price + 1)
    observeEvent(input$decreaseprice, if(rv$Price > 1){rv$Price <-rv$Price - 1})
    
    # increase or deacrease server button handled.. 
    
    observeEvent(input$addserver, rv$numberofserver <- rv$numberofserver + 1)
    observeEvent(input$lessserver, if(rv$numberofserver > 1){rv$numberofserver <-rv$numberofserver - 1})
    
    ## plot dataframe when refresh (at time of refresh)
    
    observeEvent(input$refresh, rv$x <- rv$lines)
    
    # specify the output 
    
    output$marketinglevel <- renderText(paste("your level is :", rv$marketing_level))
    output$piggy <- renderText(paste("Your Balance is:", rv$piggy_bank,"$"))
    output$mojitosupplylevel <- renderText(rv$mojito_stock_level)
    output$Price <- renderText(paste(rv$Price,"$"))
    output$profit <- renderText({paste("Your Gross Profit is: ", rv$lines[rv$n,12] ,"$")})
    output$saled_mojito <- renderText(rv$saled_mojito)
    output$numberofserver <- renderText(paste("you have:", rv$numberofserver,"waiter"))
    output$totalcost <- renderText(paste( rv$totalcost,"$"))
    output$lemonstock <- renderText(paste( rv$lemonstock,"lemon"))
    output$mintstock <- renderText(paste( rv$mintstock,"mint"))
    output$sugarstock <- renderText(paste( rv$sugarstock,"sugar"))
    output$rhumstock <- renderText(paste( rv$rhumstock,"rhum"))
    output$mojitonumber <- renderText(paste( rv$mojitonumber,"mojito"))
    
    
    
    
    ## define how much customer arrives each lambda second
    
    customer <- function(x) {
      if ( 0 < x && x <= ((2^1/1) * exp(-2)) ){ nb <- 0 }
      if ( ((2^1/1) * exp(-2)) < x  && x <= ((2^2/2) * exp(-2)) ){ nb <- 1 }
      if ( ((2^2/2) * exp(-2)) < x  && x <= ((2^3/3) * exp(-2)) ){ nb <- 2 }
      if ( ((2^3/3) * exp(-2)) < x  && x <= ((2^4/4) * exp(-2)) ){ nb <- 3 }
      if ( ((2^4/4) * exp(-2)) < x  && x <= ((2^5/5) * exp(-2)) ){ nb <- 4 }
      if ( ((2^5/5) * exp(-2)) < x  && x <= ((2^6/6) * exp(-2)) ){ nb <- 5 }
      if ( ((2^6/6) * exp(-2)) < x  && x <= ((2^7/7) * exp(-2)) ){ nb <- 6 }
      nb
    }
    
    
    observe({
      
      
      ## display the number of mojito we are able to do with this stock..
      
      rv$mojitonumber <- min(rv$rhumstock%/%1,rv$mintstock%/%2,rv$lemonstock%/%2,rv$sugarstock%/%3)
      
      ## set ambiance as a multiplier for further fct : classic = 1, electro = 2, jazz = 3, house =4
      
      rv$ambiance <- if(input$ambiance=="Classic"){
        rv$ambiance=1}else{if(input$ambiance=="Electro"){
          rv$ambiance=2}else{if(input$ambiance=="Jazz"){rv$ambiance=3}else{if(input$ambiance=="House"){rv$ambiance=4}
          }
          }
        }
    })
    
    
    
    
    
    ## create dataframe for sale..
    observe({
      
      ## set lambda
      isolate({ lambda <- (10000*rv$Price)/(rv$marketing_level*rv$ambiance)})
      
      invalidateLater(lambda, session)
      
      isolate({
        
        
        rv$n <- rv$n + 1
        rv$lines[rv$n, 1] <- rv$n                                                              ## counter n
        rv$lines[rv$n, 2] <- customer( runif(1, min = 0, max = 1) )                            ## how much customer  "enter"
        rv$lines[rv$n, 3] <- if ( rv$numberofserver>rv$lines[rv$n, 2]) {rv$lines[rv$n, 2]} else {rv$numberofserver} ##number of customer serve depending on server capacity
        rv$lines[rv$n, 4] <- if (rv$Price<=6){rv$lines[rv$n, 3]}else{if(6<rv$Price & rv$Price<8){round(rv$lines[rv$n, 3]*0.8)}else{if(8<=rv$Price & rv$Price<10){round(rv$lines[rv$n, 3]*0.6)}else{if(10<=rv$Price & rv$Price<15){round(rv$lines[rv$n, 3]*0.3)}else{if(15<=rv$Price ){round(rv$lines[rv$n, 3]*0.1)}}}}} ## number of customer served in regard to the price
        rv$lines[rv$n, 5] <- min(rv$lines[rv$n, 4],rv$mojitonumber)
        rv$lines[rv$n, 6] <- rv$Price * rv$lines[rv$n, 5]                         ## save a value of how much we earn from this sales
        rv$piggy_bank <- rv$piggy_bank + rv$Price * rv$lines[rv$n, 5]          ## impact that in piggy bank
        ## handle stock
        rv$lemonstock <- rv$lemonstock - 2*rv$lines[rv$n, 5]
        rv$lines[rv$n, 7] <- rv$lemonstock ##lemon stock after sale
        rv$mintstock <- rv$mintstock - 2*rv$lines[rv$n, 5]
        rv$lines[rv$n, 8] <- rv$mintstock ##mint stock after sale
        rv$sugarstock <- rv$sugarstock - 3*rv$lines[rv$n, 5]
        rv$lines[rv$n, 9] <- rv$sugarstock ##sugar stock after sale
        rv$rhumstock <- rv$rhumstock - 1*rv$lines[rv$n, 5]
        rv$lines[rv$n, 10] <- rv$rhumstock ##rhum stock after sale
        rv$lines[rv$n, 11] <- round(difftime(Sys.time(),initialtime,units = "secs"))                ## store the elapsed time when customer arrive
        rv$lines[rv$n, 12] <- sum( rv$lines[, 6])                                             ## store profit over time
        rv$lines[rv$n, 13] <- sum( rv$lines[, 5]) ## cumulative sale of mojito
        rv$saled_mojito <- rv$lines[rv$n, 13]
        ##set name for dataframe column
        
        colnames(rv$lines) <- c("counter", "nbr of cust entering","cust serve in regard to server","served in regard of Price","real sale depending on stock","gain of sale nbr#","lemon stock","mint stock","sugar stock","rhum stock","time in sec","cumulative profit","sold mojito")
      })
      
    })
    
    ## cost of runing your business for now just 2$ by server each 5 sec (maybe add cost for storage..)
    observe({
      
      ## input cost for each server
      
      invalidateLater(10000)
      isolate({
        rv$piggy_bank <- rv$piggy_bank - 2*rv$numberofserver
      rv$piggy_bank <- rv$piggy_bank - 0.1*rv$rhumstock
      rv$piggy_bank <- rv$piggy_bank - 0.1*rv$mintstock
      rv$piggy_bank <- rv$piggy_bank - 0.1*rv$lemonstock
      rv$piggy_bank <- rv$piggy_bank - 0.1*rv$sugarstock
      
      
     })
      
    })
    
    observe({rv$totalcost <- 2*rv$numberofserver+0.1*rv$rhumstock+0.1*rv$mintstock+0.1*rv$lemonstock+0.1*rv$sugarstock})
   
    
    ## see the data frame
    output$loc <- renderDataTable({
      
      rv$x
      
    })
    
    ##graph output
    output$myPlot <- renderPlot({
      switch(input$type, 
             Profit = ggplot() + geom_line(aes(x = rv$lines[,11], y = rv$lines[,12])) +
               xlab("Elapsed Time (in seconds)")+
               ylab("Profit over Time"),
             Number_of_mojito_sold = ggplot() + geom_line(aes(x = rv$lines[,11], y = rv$lines[,13]))+
               xlab(" Elapsed Time (in seconds)") +
               ylab("Cumulative sales of mojito"))
      
    })
    
    ## Warning no more stock to produce mojito
    observe({
      if(rv$mojitonumber<1){
        # Show a modal when the button is pressed
        shinyalert("You have no more stock to produce Mojito Bro!",
                   type = "warning",
                   callbackR = function(x) { })
      }
      
    })
    
    
    ## end of game : 
    observe({
      if(rv$lines[rv$n,12]>1000 ){
        # Show a modal when the button is pressed
        shinyalert("Yeah !!!", "You won the game making more than 1000$ profit. 
                   Click OK to close the game",
                   type = "success",
                   callbackR = function(x) { stopApp(returnValue = invisible()) })
      }
      
      if(rv$piggy_bank<5 & rv$mojitonumber<1| rv$piggy_bank< (-200)){
        # Show a modal when the button is pressed
        shinyalert("Nope !!!", "You lost the game you have no more money nor mojito to sell.
                   Click OK to close the game",
                   type = "error",
                   callbackR = function(x) { stopApp(returnValue = invisible()) })
      }
    })
    
  })
  
}


shinyApp(ui = ui, server = server)
