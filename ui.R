library(plotly)
library(shiny)
library(tidyverse)
library(readxl)

ui <- function(input, output, session) {
    fluidPage(
            fluidRow(
                column(radioButtons("group_select", "Select Group", choices = c(1,2,3, "ALL"), selected="ALL"), width=1),
                column(plotlyOutput("parcoords"), width=11)
                ),
            fluidRow(
                column(plotOutput("bar"), width=4),
                column(plotOutput("point"), width=4)
                )
    )
}
