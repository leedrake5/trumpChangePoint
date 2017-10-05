library(shinythemes)
library(shiny)
library(DT)
library(bcp)


shinyUI(navbarPage("Bayesian Change Point", id="nav", theme = shinytheme("paper"),
tabPanel("Trump Approval",
div(class="outer",
sidebarLayout(
sidebarPanel(

tags$style(type="text/css",
".shiny-output-error { visibility: hidden; }",
".shiny-output-error:before { visibility: hidden; }"
),

actionButton("trumpbayes", label = "Go"),
downloadButton('downloadtrumpplot', label="Plot"),
downloadButton('downloadtrumptable', label="Table"),

tags$p("Click 'Go' to start analysis"),



tags$hr(),


uiOutput('selectPeople'),
uiOutput('selectPollster'),
uiOutput('selectMode'),
uiOutput('selectPartisan')



),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Plot',

# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("approvalOutput", height = 400,
hover = hoverOpts("plot_hoverapproval", delay = 100, delayType = "debounce")),
uiOutput("hover_infoapproval")
),

div(
style = "position:relative",
plotOutput("posteriorProbOutput", height = 200,
hover = hoverOpts("plot_hoverposteriorprob", delay = 100, delayType = "debounce")),
uiOutput("hover_infoposteriorprob")
)



),

tabPanel("Bayes Table", DT::dataTableOutput('trumpchangepointtable')),


tabPanel("Poll Table", DT::dataTableOutput('trumppolltable'))


)
)
)
)
)
)
)
