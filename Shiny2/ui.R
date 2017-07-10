#Shiny2 UI script
require(shiny)
require(overlap)

fluidPage(
  titlePanel("Overlap Analysis"),
  tabsetPanel(
    #Upload tab
    tabPanel(title = "Upload Data",
      fluidRow(column(8,
        HTML("
<h4>Uploaded files must be in csv format. Each row represents an observation, and the following columns must be present:</h4>
<ul>
  <li>\"<b>Independent</b>\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation).</li>
  <li>\"<b>Species</b>\", which holds the <i><u>species name</u></i> of the animal in the observation. Blank, \"unknown\", and \"Unknown\" values will be removed automatically.</li>
  <li>\"<b>Time</b>\", which holds the time of day at which the photograph was taken, in decimal format, out of either 24 hours (e.g: 11:59pm -> 23.98) or 1 day (e.g: 11:59pm -> 0.99).</li>
  <li>\"<b>Survey.Name</b>\", which contains the <u>site</u>, the <u>year</u> (optional, but must be in 20xx format if present), and the <u>season</u> in which the study took place (e.g: Site1 Summer 2017, Site3 Fall 2010, Spring 2008 Site5).</li>
</ul>
<i>Note: Files with the filename \"MooringActivityRawData2.csv\" may behave somewhat strangely due to some data grouping functions that are hard-coded into the app. If you do not use that name for your file, it will act normally.</i>
<br>
<br>The source code for this app is available <u><a href=\"https://github.com/rbotts/OverlapAnalysis\">here</a></u> on Github.
             ")
      ),
      column(4,
        wellPanel(
          fileInput(inputId = "updata", label = "Upload your own data to use with the webapp:", accept = ".csv"),
          div("Once the file has finished uploading, press the button below:"),
          actionButton(inputId = "dataButton", label="Generate dataset from uploaded file")
        )
      )
      )
    ),
    
    #Two species tab
    tabPanel(title = "Two Species by Site and Season",
      fluidRow(column(3,
                      wellPanel(
                        uiOutput("2speciesUI")
                      )
                      ),
               column(9,
                      fluidRow(plotOutput("2ovlplot")),
                      fluidRow(
                        div(style = "height:20px"),
                        textOutput("2ovln"),
                        div(style = "height:20px"),
                        uiOutput("2watson"),
                        div(style = "height:20px"),
                        sliderInput("2n.boot", "Thousands of Bootstrap Samples", 1, 10, 10),
                        actionButton(inputId = "2bootButton", label="Find Confidence Interval (15+ Minutes!)"),
                        HTML("<br><b>Estimate</b>, Lower, Upper:<br>"),
                        uiOutput("2bootText"),
                        div(style = "height:20px")
                      )
                    )
      )
    ),
    
    #Single species tab
    tabPanel(title = "One Species by Site and Season",
      wellPanel(
        uiOutput("1speciesUI")
      ),
      fluidRow(
        column(8,
               plotOutput("1ovlplot")
        ),
        column(4,
               div(style = "height:20px"),
               textOutput("1ovln"),
               div(style = "height:20px"),
               uiOutput("1watson"),
               div(style = "height:20px"),
               sliderInput("1n.boot", "Thousands of Bootstrap Samples", 1, 10, 10),
               actionButton(inputId = "1bootButton", label="Find Confidence Interval (15+ Minutes!)"),
               HTML("<br><b>Estimate</b>, Lower, Upper:<br>"),
               uiOutput("1bootText"),
               div(style = "height:20px")
        )
      )
    ),
    
    #Manual tab
    tabPanel(title = "Manual Selection (Site)",
      fluidRow(column(12, HTML("<h4><font color=\"darkred\">Warning: This tab allows users to make comparisons that are not necessarily statistically meaningful. Make sure that your study design is valid before using this tab!</font></h4>"))),
      wellPanel(
        uiOutput("manUI")
      ),
      fluidRow(
        column(8,
          plotOutput("movlplot")
        ),
        column(4,
               div(style = "height:20px"),
               textOutput("movln"),
               div(style = "height:20px"),
               uiOutput("mwatson"),
               div(style = "height:20px"),
               sliderInput("mn.boot", "Thousands of Bootstrap Samples", 1, 10, 10),
               actionButton(inputId = "mbootButton", label="Find Confidence Interval (15+ Minutes!)"),
               HTML("<br><b>Estimate</b>, Lower, Upper:<br>"),
               uiOutput("mbootText"),
               div(style = "height:20px")
        )
      )
    ),
    
    #Manual Survey tab
    tabPanel(title = "Manual Selection (Survey)",
             fluidRow(column(12, HTML("<h4><font color=\"darkred\">Warning: This tab allows users to make comparisons that are not necessarily statistically meaningful. Make sure that your study design is valid before using this tab!</font></h4>"))),
             wellPanel(
               uiOutput("manUI2")
             ),
             fluidRow(
               column(8,
                      plotOutput("movlplot2")
               ),
               column(4,
                      div(style = "height:20px"),
                      textOutput("movln2"),
                      div(style = "height:20px"),
                      uiOutput("mwatson2"),
                      div(style = "height:20px"),
                      sliderInput("mn.boot2", "Thousands of Bootstrap Samples", 1, 10, 10),
                      actionButton(inputId = "mbootButton2", label="Find Confidence Interval (15+ Minutes!)"),
                      HTML("<br><b>Estimate</b>, Lower, Upper:<br>"),
                      uiOutput("mbootText2"),
                      div(style = "height:20px")
               )
             )
    )
  )
)
