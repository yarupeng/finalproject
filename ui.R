
library(shiny)
library(shinydashboard)
library(rtweet)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(plotly)
library(readr)

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

dashboardPage(
  dashboardHeader(title ="Final Project"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "#Hashtag usage",
        width = 12,
        collapsible = T,
        tabsetPanel(type = "tabs",
                    tabPanel("Adriana Lima", plotlyOutput("adriana.p")),
                    tabPanel("Martha Hunt", plotlyOutput("martha.p")),
                    tabPanel("GiGi Hadid" , plotlyOutput("gigi.p")) ,
                    tabPanel("Bella Hadid" , plotlyOutput("bella.p")) ,
                    tabPanel("Kendall Jenner" , plotlyOutput("kendall.p"))
                    
      )

      )
    ),
    fluidRow(
      box(title = "Timeline",
          width = 12,
          collapsible = T,
          plotlyOutput("timeline")
      ),
      box(title = "Friends VS Follwer table",
          width = 12,
          collapsible = T,
          tableOutput("ff")
      )
    )
  )
)

