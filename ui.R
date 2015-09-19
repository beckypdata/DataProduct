library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Corporate Hiring Process"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(  
          column(6, 
            textInput("text1", label = h5("Candidate Name"), 
                      value = "Enter name here...")
          ),
          column(6,helpText("STEP ONE: Enter candidate full name then 
                            select rating within each interview evaluated
                            category"))
        ),
        fluidRow(  
          column(4,
                 selectInput("select1", label = h5("Communication"), 
                             choices = list("Minimal" = 1, "Below Average" = 2,
                                            "Average" = 3, "Above Average" = 4,
                                            "Superior" = 5), selected = 1)),
          column(4,
                 selectInput("select2", label = h5("Problem Solving"), 
                             choices = list("Minimal" = 1, "Below Average" = 2,
                                            "Average" = 3, "Above Average" = 4,
                                            "Superior" = 5), selected = 1)),
          column(4,
                 selectInput("select3", label = h5("Integrity"), 
                             choices = list("Minimal" = 1, "Below Average" = 2,
                                            "Average" = 3, "Above Average" = 4,
                                            "Superior" = 5), selected = 1))
        ),
        fluidRow(column(3,actionButton("submit", "Submit")),
                 column(9,helpText("STEP TWO: Press Submit to calculate predicted performance 
                                   as employee and obtain hiring decision"))
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Summary",
                   fluidRow(br()),
                   fluidRow(column(3,h5('Predicted Performance:')),
                            column(3,verbatimTextOutput("select1"))),
                   fluidRow(column(3,h5('Hiring Decision:')),
                            column(3,verbatimTextOutput("decision"))),
                   fluidRow(br()),
                   fluidRow(br()),
                   fluidRow(column(6,helpText("STEP THREE: 
                            Select 'Employees' to compare candidate to current employees")))
          ),
          tabPanel("Employees",
                   h3("Comparison to Current Employees"),
                   fluidRow(column(6,plotOutput('empHist')),
                            column(6,helpText("Candidate results within each category are represented by red line"))),
                   fluidRow(column(12,helpText("STEP FOUR: Select 'Candidates' tab to compare candidate to previously 
                                              entered candidates")))
          ),
          tabPanel("Candidates", 
                   h3("Comparison to Other Candidates"),
                   fluidRow(column(6,plotOutput('candHist')),
                            column(6,helpText("Candidate results within each category are represented by red line"))),
                   fluidRow(column(12,helpText("STEP FIVE: Continue entering candidates.  Obtain hiring decision on 
                                              'Summary' tab, use 'Employees' and 'Candidates' tabs to 
                                              compare with those populations"))),               
                   fluidRow(column(12,tableOutput('table')))
          )
        )
      )
    )
  )
)
