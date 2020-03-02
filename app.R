# Rev 01 Puzzle Counter
# Decrements pieces remaining when a button is clicked

library(shiny)
library(shinyjs)
library(ggplot2)
puzzles <- readRDS('data/puzzles.rds')
options(digits.secs = 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Enable shinyjs
   useShinyjs(),
   
   # Application title
   titlePanel("Click Button Puzzle Counter"),
   
   # Sidebar that calls annyang voice recognition library and js script then
   # has inputs for puzzle name, total number of pieces, desired keyword and
   # buttons to start the puzzle, place a piece, or save your data.
   # Lastly a data table displays the most recent pieces placed.
   sidebarLayout(
      sidebarPanel(
        tags$div(
          tags$script(
            src = '//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js'),
          includeScript('voice.js')
          ),
        textInput('name', 'Puzzle name:',
                  value = ''),
        textInput('total', 'Pieces in puzzle:',
                  value = ''),
        textInput('keyword', "Keyword for voice recognition:",
                  value = 'check'),
        br(),
        span(disabled(actionButton('start', 'Begin working', width = 130)),
             disabled(actionButton('click', 'Piece placed'))),
        br(),
        br(),
        disabled(actionButton('save', 'Save Data to File')),
        br(),
        br(),
        tableOutput('table')
      ),
      
      # Show a plot pieces remaining vs cumulative work time
      mainPanel(
        plotOutput('pvt_plot')
      )
   )
)

# Define server logic
server <- function(input, output, session) {
  
   vals <- reactiveValues()
   
   # Observer for 'start' button press.
   # If number of presses is odd, start working on puzzle:
   # disable name and pieces inputs, enable click button
   # and relabel start button, set start/unpause time.
   # If this is the first press initialize the dataframe
   # for the current puzzle.
   # If number of presses is even, pause working on puzzle:
   # disable click button and relabel start button.
   observeEvent(input$start, {
     if (input$start%%2 == 1){
       updateActionButton(session, 'start', label = 'Pause')
       disable('total')
       disable('name')
       enable('click')
       vals$init_time <- as.numeric(Sys.time())
       if (input$start == 1){
         vals$p = puzzles[puzzles$name == input$name,]
         if (length(which(puzzles$name == input$name)) == 0){
           vals$p <-data.frame(name = as.character(input$name), pieces = as.numeric(input$total), 
                               time=Sys.time(), interval=0, cumul=0)
         }
       }
     }
     else {
       updateActionButton(session, 'start', label = 'Resume')
       disable('click')
     }
   })
  
  observeEvent(input$click, {
    enable('save')
    isolate({
      now = as.numeric(Sys.time())
      past = max(as.numeric(vals$p$time), vals$init_time)
      vals$p = rbind(vals$p, rep(NA,5))
      rows = nrow(vals$p)
      vals$p$interval[rows] = now - past
      vals$p$time[rows] = Sys.time()
      vals$p$pieces[rows] = vals$p$pieces[rows-1]-1
      vals$p$name[rows] = input$name
      vals$p$cumul = cumsum(vals$p$interval)
    })
  })
  
  observeEvent(input$spoken, {
    if (input$spoken == input$keyword & input$start%%2 == 1){
      enable('save')
      insertUI(selector = '#name', where = 'afterEnd',
               ui = tags$audio(src = "bell.mp3", type = "audio/mp3", 
                               autoplay = NA, controls = NA, style="display:none;"))
      isolate({
        now = as.numeric(Sys.time())
        past = max(as.numeric(vals$p$time), vals$init_time)
        vals$p = rbind(vals$p, rep(NA,5))
        rows = nrow(vals$p)
        vals$p$interval[rows] = now - past
        vals$p$time[rows] = Sys.time()
        vals$p$pieces[rows] = vals$p$pieces[rows-1]-1
        vals$p$name[rows] = input$name
        vals$p$cumul = cumsum(vals$p$interval)
      })
    }
  })
  
   observeEvent(input$name, {
     if (input$name != "" & !is.na(as.integer(input$total))){
       enable('start')
     }
   })
   
   observeEvent(input$total, {
     if (!is.na(as.integer(input$total))) {
       pieces = as.integer(input$total)
       if (input$name != ""){
         enable('start')
       }
     }
     else {
       updateTextInput(session, 'total', value = '')
     }
   })
   
   output$pvt_plot <- renderPlot({
     if (input$start >= 1){
       df = rbind(puzzles[puzzles$name != input$name,], vals$p)
     }
     else {
       df = puzzles
     }
     pplot <- ggplot(df, aes(x=cumul/3600, y=pieces, group=name, color=name)) +
       geom_line(size = 1) +
       xlab('Cumulative time, hours') + ylab('Pieces remaining') +
       scale_x_continuous(breaks = seq(0, 100, by=1)) +
       labs(color = 'Puzzle name') + theme_minimal(base_size = 17) + 
       theme(aspect.ratio = 1)
     pplot
   })
   
   output$table <- renderTable({
     #puzzles$time <- format(puzzles$time, '%Y-%m-%d %h')
     #vals$time = puzzles$time
     if(input$start >= 1){
       df = data.frame(Name = vals$p$name, Pieces = as.integer(vals$p$pieces), 
                        Time = format(vals$p$time), Interval = vals$p$interval,
                        Cumulative = vals$p$cumul)
     }
     else {
       df = puzzles
       df$pieces = as.integer(df$pieces)
       df$time = format(df$time)
       names(df) <- c('Name', 'Pieces', 'Time', 'Interval', 'Cumulative')
     }
     tail(df)
   })
   
   observeEvent(input$save, {
     saveRDS(vals$p, paste('data/',vals$p$name[1],'.rds', sep=''))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

