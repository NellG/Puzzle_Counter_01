# Rev 01 Puzzle Counter
# Decrements pieces remaining when a button is clicked 
# or the voice keyword is spoken (works in Chrome).

library(shiny)
library(shinyjs)
library(ggplot2)
library(rdrop2)

token = readRDS('data/token.rds')
drop_download('puzzles.rds', overwrite = TRUE, 
              local_path = 'data/puzzles.rds',
              dtoken = token)
puzzles <- readRDS('data/puzzles.rds')
options(digits.secs = 2)

updatedf<- function(df, init_time){
  now = as.numeric(Sys.time())
  past = max(as.numeric(df$time), init_time)
  df = rbind(df, rep(NA,5))
  rows = nrow(df)
  df$interval[rows] = now - past
  df$time[rows] = Sys.time()
  df$pieces[rows] = df$pieces[rows-1]-1
  df$name[rows] = df$name[rows-1]
  df$cumul = cumsum(df$interval)
  return(df)
}

formatdf <- function(df){
  dfout = data.frame(Name = df$name, Pieces = as.integer(df$pieces), 
                  Time = format(df$time), Interval = df$interval)
  return(dfout)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Enable shinyjs
   useShinyjs(),
   
   # Application title
   titlePanel(h1("Jigsaw Puzzle Progress Timer App", align = 'center')),
   
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
        fileInput('upfile', 'Upload previously saved puzzle data to continue working:', 
                  multiple = FALSE, accept = '.rds'),
        textInput('name', 'Puzzle name:',
                  value = ''),
        textInput('total', 'Pieces in puzzle:',
                  value = ''),
        textInput('keyword', "Keyword for voice recognition:",
                  value = 'check'),
        helpText('Voice recognition works best (only) in Chrome on a PC.'),
        br(),
        span(disabled(actionButton('start', 'Begin working', width = 130)),
             disabled(actionButton('click', 'Piece placed'))),
        helpText('If the "Piece placed" button is selected you can also
                  hit spacebar to record a piece placement.'),
        br(),
        disabled(downloadButton('save', 'Download Data to File')),
        br(),
        br(),
        tableOutput('table')
      ),
      
      # Show a plot pieces remaining vs cumulative work time
      mainPanel(
        p('Welcome to the jigsaw puzzle counter! Please enter the name
          of your puzzle and the number of pieces or upload the saved file
          from a puzzle you have worked on previously. Your puzzle progress 
          will be plotted below. If you would like your puzzle data included in
          the main plot for all users, and in any future statistical analysis, please email the
          data file for the completed puzzle to: nellspuzzles.at.gmail.', 
          style = "font-size: 14pt"),
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
         if (input$name != ''){
           vals$p = puzzles[puzzles$name == input$name,]
           if (length(which(puzzles$name == input$name)) == 0){
               vals$p <-data.frame(name = as.character(input$name), pieces = as.numeric(input$total), 
                                   time=Sys.time(), interval=0, cumul=0)
           }
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
      vals$p = updatedf(vals$p, vals$init_time)
    })
  })
  
  observeEvent(input$spoken, {
    if (input$spoken == input$keyword & input$start%%2 == 1){
      enable('save')
      insertUI(selector = '#name', where = 'afterEnd',
               ui = tags$audio(src = "bell.mp3", type = "audio/mp3", 
                               autoplay = NA, controls = NA, style="display:none;"))
      isolate({
        vals$p = updatedf(vals$p, vals$init_time)
      })
    }
  })
  
  observeEvent({input$name
                input$total}, {
    if (!is.na(as.integer(input$total))){
      pieces = as.integer(input$total)
      if (input$name != ''){
        enable('start')
        disable()
      }
    }
    else {
      updateTextInput(session, 'total', value='')
    }
  })
  
  observeEvent(input$upfile, {
    vals$p = readRDS(input$upfile$datapath)
    disable('total')
    disable('name')
    enable('start')
  })
   
   output$pvt_plot <- renderPlot({
     if (input$start >= 1){ df = rbind(puzzles[puzzles$name != input$name,], vals$p) }
     else { df = puzzles }
     
     ggplot(df, aes(x=cumul/3600, y=pieces, group=name, color=name)) +
       geom_line(size = 1) +
       xlab('Cumulative time, hours') + ylab('Pieces remaining') +
       scale_x_continuous(breaks = seq(0, 100, by=1)) +
       labs(color = 'Puzzle name') + theme_minimal(base_size = 17) + 
       theme(aspect.ratio = 1)
   })
   
   output$table <- renderTable({
     if(input$start >= 1){ df = vals$p }
     else { df <- puzzles }
     tail(formatdf(df), n = 3)
   })
   
   output$save <- downloadHandler(
     filename = function() {
       paste(input$name, '-', Sys.Date(), '.rds', sep='')
     },
     content = function(file) {
       saveRDS(vals$p, file)
     }
   )
   #observeEvent(input$save, {
  #   saveRDS(vals$p, paste('data/',vals$p$name[1],'.rds', sep=''))
   #})
}

# Run the application 
shinyApp(ui = ui, server = server)

