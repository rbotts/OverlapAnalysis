#Shiny app for lunar analysis
require(shiny)

fluidPage(
  titlePanel("Lunar Analysis"),
  tabsetPanel(
    
    #Upload tab ----
    tabPanel(title = "Upload Data",
             fluidRow(column(8, HTML(
                               "
                               <h4>Uploaded files must be in csv format. Each row represents an observation, and the following columns must be present:</h4>
                               <ul>
                               <li>\"<b>Independent</b>\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation).</li>
                               <li>\"<b>Species</b>\", which holds the <i><u>species name</u></i> of the animal in the observation. Blank, \"unknown\", and \"Unknown\" values will be removed automatically.</li>
                               <li>\"<b>Date</b>\", which holds the date on which the photograph was taken, in the format \"mm/dd/yyyy\".</li>
                               <li>\"<b>Survey.Name</b>\", which contains the <u>site</u>, the <u>year</u> (optional, but must be in 20xx format if present), and the <u>season</u> in which the study took place (e.g: Site1 Summer 2017, 2012 Site3 Fall, Spring 2008 Site5).</li>
                               </ul>
                               <br>
                               <br>The source code for this app is available <u><a href=\"https://github.com/rbotts/OverlapAnalysis\">here</a></u> on Github.
                               "
                             )
                             ), 
             column(4,
                    wellPanel(
                      fileInput(inputId = "updata", label = "Upload your own data to use with the webapp:", accept = ".csv"),
                      div("Once the file has finished uploading, press the button below in order to import the data into the app:"),
                      fluidRow(column(12, align = "center",
                                      actionButton(inputId = "dataButton",
                                                   label = "Generate dataset",
                                                   width = "100%")
                      )),
                      div(style = "height:20px"),
                      div("After generating the dataset, optionally press the button below in order to use only data from after 1800 and before 0600:"),
                      fluidRow(column(12, align = "center",
                                      actionButton(inputId = "nocturnalButton",
                                                   label = "Remove daytime data",
                                                   width = "100%")
                      ))
                    )
             )
             )
    ),
    
    #Pattern Analysis tab ----
    tabPanel(title="Pattern Analysis",
      fluidRow(
        column(4, uiOutput("patternSelect")),
        column(8,
          fluidRow(plotOutput("patternGraph")),
          fluidRow(
            div(style = "height:20px"),
            textOutput("patternN"),
            div(style = "height:20px"),
            uiOutput("patternRao")
          )
        )
      )
    ),
    
    #Lunar Overlap tab ----
    tabPanel(title="Lunar Overlap",
             fluidRow(column(12, HTML("<h4><font color=\"darkred\">Warning: This tab allows users to make comparisons that are not necessarily statistically meaningful. Make sure that your study design is valid before using this tab!</font></h4>"))),
             fluidRow(
               uiOutput("overlapSelect")
             ),
             fluidRow(
               column(8,
                      plotOutput("overlapPlot")
               ),
               column(4,
                      div(style = "height:20px"),
                      textOutput("overlapN"),
                      div(style = "height:20px"),
                      uiOutput("overlapWatson"),
                      div(style = "height:20px"),
                      sliderInput("overlapBoot", "Thousands of Bootstrap Samples", 1, 10, 10),
                      actionButton(inputId = "overlapButton", label="Find Confidence Interval (15+ Minutes!)"),
                      HTML("<br><b>Estimate</b>, Lower, Upper:<br>"),
                      uiOutput("overlapCI"),
                      div(style = "height:20px")
               )
             )
    )
  )
)
