####Assigning text to variables
Background<-'Hi everyone! This is an extension of the tireless game Rock, 
Papers, Scissors! But derived from Dr. Sheldon Coopers idea from The
Bang Theory! The same rules apply, but the with the added twist of two new elements.
In case you forgot the rules here they are again:'

Rules<-tags$div(
  "RULES",
  tags$br(),
  "1. Scissors cuts paper", 
  tags$br(),tags$br(),
  "2. Paper covers rock",
  tags$br(),tags$br(),
  "3. Rock crushes lizard",
  tags$br(),tags$br(),
  "4. Lizard poisons Spock",
  tags$br(),tags$br(),
  "5. Spock smaahes scissors",
  tags$br(),tags$br(),
  "6. Scissors decapitates lizard",
  tags$br(),tags$br(),
  "7. Lizard eat paper",
  tags$br(),tags$br(),
  "8. Paper disproves Spock...WTF DOES THAT EVEN MEAN",
  tags$br(),tags$br(),
  "9. Spock vaproizes rock",
  tags$br(),tags$br(),
  "10. Rock crushes scissors",
  tags$br(),tags$br()
)

picture<-img(src='RPSLS.png')
picture2<-img(src='2.gif')
  
ui <- fluidPage(
  
  titlePanel("Rock, Paper, Scissors, Lizards, Spock, GO!"),
  
  sidebarLayout(
    sidebarPanel(
      strong("Please select Rock, Paper, Scissors,Lizard, or Spock:"),
      tags$br(),
      actionButton("rockBtn","Rock"),
      actionButton("paperBtn","Paper"),
      tags$br(),
      actionButton("scissorsBtn","Scissors"),
      actionButton("lizardsBtn","Lizard"),
      tags$br(),
      actionButton("spockBtn","Spock"),Rules
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Background",h4(Background),picture2,picture), 
        tabPanel("Weight Plot and Scoreplot", fluidRow(
          textOutput("statusTextOutput"),
          plotOutput("weightPlot")
        ),fluidRow(
          textOutput("statusTextOutput2"),
          plotOutput("scorePlot")
        )), 
        tabPanel("Win/Loss/Tie Counts", tableOutput("WinLossTied"),
                 plotOutput("WinLossTiedBar"),tableOutput("History"))
      )
    )
  )
)