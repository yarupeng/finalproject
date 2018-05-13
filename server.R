
library(shiny)
library(shinydashboard)
library(rtweet)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(plotly)
library(readr)


shinyServer(function(input, output) {
  
  # Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
  consumer_key <- "5Xtc6aCLL74G8bzSq6DLdzT5K"
  consumer_secret <- "H2sle8DmuoHi1K7JYFpinOn4Y7wl2oUnT3j7Il3OXZWvkkhVcI"
  access_token <- "992830297036144640-m6N0sExVoiyKMBOI3WURG1Pim2pEwBd"
  access_secret <- "YuTocZ2LBK7c46EwuWe64Dyy5uwjtxTB5kfYYcbN4VEaq"
  
  createTokenNoBrowser<- function(appName, consumerKey, consumerSecret, 
                                  accessToken, accessTokenSecret) {
    app <- httr::oauth_app(appName, consumerKey, consumerSecret)
    params <- list(as_header = TRUE)
    credentials <- list(oauth_token = accessToken, 
                        oauth_token_secret = accessTokenSecret)
    token <- httr::Token1.0$new(endpoint = NULL, params = params, 
                                app = app, credentials = credentials)
    return(token)
  }
  
  token <- createTokenNoBrowser("ma415 -finalproject",consumer_key, consumer_secret, access_token, access_secret)
  
  output$adriana.p <- renderPlotly({
    
    adriana <- search_tweets("#adrianalima", n = 1000, include_rts = FALSE,token = token)
    
    ts_plot(adriana, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of #adrianalima Twitter statuses from past month",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    
  })
  
  output$martha.p <- renderPlotly({
    martha <- search_tweets("#marthahunt", n = 1000, include_rts = FALSE,token = token)
    
    ts_plot(martha, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of #marthahunt Twitter statuses from past month",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    
  })
  
  output$gigi.p <- renderPlotly({
    gigi <- search_tweets("#gigihadid", n = 1000, include_rts = FALSE,token = token)
    
    ts_plot(gigi, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of #gigihadid Twitter statuses from past month",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    
  })
  
  output$bella.p <- renderPlotly({
    
    bella <- search_tweets("#bellahadid", n = 1000, include_rts = FALSE,token = token)
    
    ts_plot(bella, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of #bellahadid Twitter statuses from past month",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    
  })
  
  output$kendall.p <- renderPlotly({
    kendall <- search_tweets("#kendalljenner", n = 1000, include_rts = FALSE,token = token)
    
    ts_plot(kendall, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of #kendalljenner Twitter statuses from past month",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    
    
  })
  
  output$timeline <- renderPlotly({
    
    tmls <- get_timeline(c("AdrianaLima", "MarthaHunt","bellahadid","GiGiHadid","KendallJenner"), n=1000, token = token)
    tmls %>%
      dplyr::filter(created_at > "2017-05-05") %>%
      dplyr::group_by(screen_name) %>%
      ts_plot("days", trim = 1L) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of Twitter statuses posted by Models",
        subtitle = "Twitter status (tweet) counts aggregated by day from June-April 2018",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )

  })
  
  output$ff <- renderTable({
    user <- lookup_users(c("AdrianaLima", "MarthaHunt","bellahadid","GiGiHadid","KendallJenner"), token = token)
    
    ff.table <- rbind(user$followers_count,user$friends_count)
    colnames(ff.table) <- c("AdrianaLima", "MarthaHunt","bellahadid","GiGiHadid","KendallJenner")
    row.names(ff.table) <- c("follower count", "friends count")
    
    ff.table
    
  })
  
})