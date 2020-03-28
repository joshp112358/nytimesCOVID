
library(dplyr)
library(shiny)
library(readr)

shinyServer(function(input, output) {
  # ---------------- Download Data ----------------
  
  url_nytimes_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  url_nytimes_states <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

  counties <- read_csv(url(url_nytimes_counties))
  counties$date = as.Date(counties$date, format="%y/%m/%d")
  
  states <- read_csv(url(url_nytimes_states))
  states$date = as.Date(states$date, format="%y/%m/%d")
  
  # ---------------- UI ------------------
  output$ui1 <- renderUI({
    selectInput("States", 
                "States",
                sort(unique(states$state)),
                "New York")
  })
  output$ui2 <- renderUI({
    # if (input$States=="All"){
    #   selectInput("County",
    #               "County",
    #               c("All"),
    #               "All")
    # }
    # else{
      l<-sort(unique(filter(counties, state==input$States)$county))
      selectInput("County",
                  "County",
                  c("All",l),
                  "All")
    # }
  })
  # --------------- Plot Graphic ----------------
  
  ## -------------- CD Text Output -----------------
  output$ConfirmedDead <- renderText({
    if (input$County == "All"){
      data_state = filter(states, state == input$States)
      paste("As of", tail(data_state$date, 1),
            "the number of confirmed cases in", input$States, "is", tail(data_state$cases, 1),
            "and the number of deaths is", tail(data_state$deaths, 1))
    }
    else {
      data_state = filter(counties, county == input$County)
      paste("As of", tail(data_state$date, 1),
            "the number of confirmed cases in", input$County, "is", tail(data_state$cases, 1),
            "and the number of deaths is", tail(data_state$deaths, 1))
    }
  })
  ## -------------- Hist CD ----------------
  output$HistCD <- renderPlot({
    if (input$County == "All"){
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(log(data_state$cases))~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Count of Log Scale", input$States),
               xlab = "Time",
               ylab = "Log Count",
               col = "red")
          points(diff(log(data_state$deaths))~data_state$date[2:length(data_state$date)],
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
        else {
          data_state = filter(states, state == input$States)
          plot(log(data_state$cases)~data_state$date,
               type = "o",
               main = paste("Log Scale", input$States),
               xlab = "Time",
               ylab = "Log Count",
               col = "red")
          points(log(data_state$deaths)~data_state$date,
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(data_state$cases)~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Count", input$States),
               xlab = "Time",
               ylab = "Count",
               col = "red")
          points(diff(data_state$deaths)~data_state$date[2:length(data_state$date)],
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
        else {
          data_state = filter(states, state == input$States)
          plot(data_state$cases~data_state$date,
               type = "o",
               main = input$States,
               xlab = "Time",
               ylab = "Count",
               col = "red")
          points(data_state$deaths~data_state$date,
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
      }
    }
    #not All counties
    else {
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(log(data_county$cases))~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Count of Log Count", input$County),
               xlab = "Time",
               ylab = "Log Count",
               col = "red")
          points(diff(log(data_county$deaths))~data_county$date[2:length(data_county$date)],
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(log(data_county$cases)~data_county$date,
               type = "o",
               main = paste("Log Count",input$County),
               xlab = "Time",
               ylab = "Log Count",
               col = "red")
          points(data_county$deaths~data_county$date,
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(data_county$cases)~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Count", input$County),
               xlab = "Time",
               ylab = "Count",
               col = "red")
          points(diff(data_county$deaths)~data_county$date[2:length(data_county$date)],
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(data_county$cases~data_county$date,
               type = "o",
               main = input$County,
               xlab = "Time",
               ylab = "Count",
               col = "red")
          points(data_county$deaths~data_county$date,
                 type = "o",
                 col = "black")
          legend("topleft", 
                 legend=c("Confirmed Cases", "Deaths"),
                 col=c("red","black"),
                 lty=1:1, 
                 cex=0.8)
        }
      }
    }
  })
  
  #Hist Dead 
  
  ## -------------- CD Text Output -----------------
  output$DeathToll <- renderText({
    if (input$County == "All"){
      data_state = filter(states, state == input$States)
      paste("As of", tail(data_state$date, 1),
            "the number of deaths in", input$States, "is", tail(data_state$deaths, 1))
    }
    else {
      data_state = filter(counties, county == input$County)
      paste("As of", tail(data_state$date, 1),
            "the number of deaths in", input$County,"is", tail(data_state$deaths, 1))
    }
  })
  ## ------------ Hist Dead --------------
  
  
  output$HistDead <- renderPlot({
    if (input$County == "All"){
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(log(data_state$deaths))~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Count of Log Deaths in", input$States),
               xlab = "Time",
               ylab = "Log Count",
               col = "black")
        }
        else {
          data_state = filter(states, state == input$States)
          plot(log(data_state$deaths)~data_state$date,
               type = "o",
               main = paste("Log Count Deaths in", input$States),
               xlab = "Time",
               ylab = "Log Count",
               col = "black")
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(data_state$deaths)~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Count of Deaths in", input$States),
               xlab = "Time",
               ylab = "Count",
               col = "black")
        }
        else {
          data_state = filter(states, state == input$States)
          plot(data_state$deaths~data_state$date,
               type = "o",
               main = paste("Deaths in", input$States),
               xlab = "Time",
               ylab = "Count",
               col = "black")
        }
      }
    }
    #not All counties
    else {
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(log(data_county$deaths))~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Count of Log Scale Deaths in", input$County),
               xlab = "Time",
               ylab = "Log Count",
               col = "black")
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(log(data_county$deaths)~data_county$date,
               type = "o",
               main = paste("Log Scale Deaths in", input$County),
               xlab = "Time",
               ylab = "Log Count",
               col = "black")
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(data_county$deaths)~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Count  of Deaths in", input$County),
               xlab = "Time",
               ylab = "Count",
               col = "black")
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(data_county$deaths~data_county$date,
               type = "o",
               main = paste("Deaths in", input$County),
               xlab = "Time",
               ylab = "Count",
               col = "black")
        }
      }
    }
  })
  ## -----------Dead/confirmed ----------
  output$NaiveMortality <- renderPlot({
    if (input$County == "All"){
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(log(data_state$deaths/data_state$cases*100))~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Change of Log of Case Fatality Ratio", input$States),
               xlab = "Time",
               ylab = "Log Percentage",
               col = "black")
        }
        else {
          data_state = filter(states, state == input$States)
          plot(log(data_state$deaths/data_state$cases*100)~data_state$date,
               type = "o",
               main = paste("Log of Case Fatality Ratio", input$States),
               xlab = "Time",
               ylab = "Log Percentage",
               col = "black")
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_state = filter(states, state == input$States)
          plot(diff(data_state$deaths/data_state$cases*100)~data_state$date[2:length(data_state$date)],
               type = "o",
               main = paste("Daily Change in Case Fatality Ratio", input$States),
               xlab = "Time",
               ylab = "Percentage",
               col = "black")
        }
        else {
          data_state = filter(states, state == input$States)
          plot(data_state$deaths/data_state$cases*100~data_state$date,
               type = "o",
               main = paste("Case Fatality Ratio", input$States),
               xlab = "Time",
               ylab = "Percentage",
               col = "black")
        }
      }
    }
    #not All counties
    else {
      if (input$Log == TRUE) {
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(log(data_county$deaths/data_county$cases*100))~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Change of Log of Case Fatality Ratio", input$County),
               xlab = "Time",
               ylab = "Log Percentage",
               col = "black")
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(log(data_county$deaths/data_county$cases*100)~data_county$date,
               type = "o",
               main = paste("Log of Case Fatality Ratio", input$County),
               xlab = "Time",
               ylab = "Log Percentage",
               col = "black")
        }
      }
      else{
        if (input$First_Derivative == TRUE){
          data_county = filter(counties, state == input$States & county == input$County)
          plot(diff(data_county$deaths/data_county$cases*100)~data_county$date[2:length(data_county$date)],
               type = "o",
               main = paste("Daily Change of Case Fatality Ratio", input$County),
               xlab = "Time",
               ylab = "Percentage",
               col = "black")
        }
        else {
          data_county = filter(counties, state == input$States & county == input$County)
          plot(data_county$deaths/data_county$cases*100~data_county$date,
               type = "o",
               main = paste("Case Fatality Ratio", input$County),
               xlab = "Time",
               ylab = "Percentage",
               col = "black")
        }
      }
    }
  })
})
