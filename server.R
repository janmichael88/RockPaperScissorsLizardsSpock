#################################################################
####Server
#################################################################
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
source("RockPaperScissorsUtilities.R")


shinyServer(function(input, output) {
  v <- reactiveValues()
  
  
  #Initialize the scores
  v$weights <-c(1,1,1,1,1)
  probs<-reactive(v$weights / sum(v$weights))
  v$score <- data.frame(competitor = c("Human","Computer","Tied"), Score = c(0,0,0))
  v$scoreHistory <- data.frame(Human = 0, Computer = 0, Tied = 0)
  runComputation <- function(rps)({
    compResult <- sample(1:5, 1, prob= v$weights, replace = TRUE)
    v$computationResult <-  paste("Human:",convertToGame(rps),"/ Computer:",convertToGame(compResult))
    
    if (compResult ==  rps)
    {
      v$computationResult <- paste("You have tied using",convertToGame(compResult))
      v$score <- tallyScore(compResult, rps,0,v$score)
    }
    else {
      #Rock (1) Beats Scissors (3)
      if ((compResult %in% c(1,3) && rps %in% c(1,3)))
      { 
        trump <- 1
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Rock (1) Beats Lizard (4)
      if ((compResult %in% c(1,4) && rps %in% c(1,4)))
      { 
        trump <- 1
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Spock (5) Beats Rock (1)
      if ((compResult %in% c(1,5) && rps %in% c(1,5)))
      { 
        trump <- 5
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Lizard (4) Beats Paper (2)
      if ((compResult %in% c(2,4) && rps %in% c(2,4)))
      { 
        trump <- 4
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Spock (5) Beats Paper (2)
      if ((compResult %in% c(2,5) && rps %in% c(2,5)))
      { 
        trump <- 2
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Scissors (3) Beats Lizard (4)
      if ((compResult %in% c(3,4) && rps %in% c(3,4)))
      { 
        trump <- 3
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Spock (5) Beats Scissors (3)
      if ((compResult %in% c(3,5) && rps %in% c(3,5)))
      { 
        trump <- 5
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #Lizards (4) Beats Spock (5)
      if ((compResult %in% c(4,5) && rps %in% c(4,5)))
      { 
        trump <- 4
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      # Scissors (3) Beats Paper (2)
      if ((compResult %in% c(2,3) && rps %in% c(2,3)))
      {
        trump <- 3
        v$statusTextOutput <- printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
      #  Paper (2) Beats Rock(1)
      if ((compResult %in% c(1,2) && rps %in% c(1,2)))
      {
        trump <- 2
        v$statusTextOutput <-  printWinner(compResult,rps,trump)
        v$score <- tallyScore(compResult,rps,trump,v$score)
        v$weights <- updateWeights(v$weights,trump)
      }
    }
    v$scoreHistory <- rbind(v$scoreHistory, c(v$score$Score[1],v$score$Score[2],v$score$Score[3]))
  })
  
  #Checks to see which button was pressed
  observeEvent(input$rockBtn, { runComputation(1)})
  observeEvent(input$paperBtn, {runComputation(2)})
  observeEvent(input$scissorsBtn, {runComputation(3)})
  observeEvent(input$lizardsBtn, {runComputation(4)})
  observeEvent(input$spockBtn, {runComputation(5)})
  
  #### Output ####
  output$statusTextOutput <- renderText(v$computationResult)
  
  output$statusTextOutput2 <- renderText(printScore(v$score$Score[1],v$score$Score[2],v$score$Score[3]))
  
  output$scorePlot <- renderPlot(
    ggplot(v$scoreHistory) + geom_point(aes(x = Human, y = Computer)) 
  )
  
  output$WinLossTied <- renderTable(v$score)
  
  output$History<-renderTable(v$scoreHistory)
  
  output$WinLossTiedBar<- renderPlot(
    ggplot(v$score,aes(x=competitor,y=Score,fill=competitor)) + geom_bar(stat='identity')
  )
  
  output$weightPlot <- renderPlot({
    final <- data.frame(Type = c("Rock","Paper","Scissors",'Lizard','Spock'), Weights = v$weights/sum(v$weights))
    plot(final)
  })
  
})