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

sliderInput("prior", label="Prior Probability of a change-point", min=0.001, max=1, value=0.001),
tags$p("A smaller prior is better for big data sets"),

tags$hr(),

uiOutput('selectPeople'),
uiOutput('selectPollster'),
uiOutput('selectMode'),
uiOutput('selectPartisan'),
tags$p("Choose your inputs, but keep in mind live telephone interviews are more reliable")




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
