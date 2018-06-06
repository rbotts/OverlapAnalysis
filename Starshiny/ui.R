#R Version 3.4.4
#Starshiny UI script

require("shiny")

fluidPage(
  
  #Making the active tab a nice shade of orange...
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: white;  color:cornflowerblue}
    .tabbable > .nav > li[class=active]    > a {background-color: #EB6536; color:white}
  ")),
  
  titlePanel(""), #For spacing
  
  #Main content
  tabsetPanel(type = "pills",
    tabPanel(title = "Upload Data", #Upload tab ----
      titlePanel(title = "Starshiny: Overlap Analysis with R Shiny", windowTitle = "Starshiny"),
      fluidRow(
        column(4,
          HTML("This web app allows users study the patterns in and overlap between circular density curves. It is designed to be used to study the activity patterns of animals based on camera trap data, although it should also work with any other type of cyclically-dependent detection data. <br> <br>"),
          wellPanel(
            fileInput(inputId = "dataFile",
                      label = NULL,
                      accept = ".csv",
                      width = "100%",
                      placeholder = "Select data csv"),
            radioButtons(inputId = "modeSelect",
                         label = "What would you like to use as your (x-axis) independent variable?",
                         choiceNames = c("Clock Time", "Solar Time", "Moon Phase"),
                         choiceValues = c("clock", "solar", "lunar"))
          )
        ),
        column(8,
          HTML(
            "<b>Instructions:</b> <br>
            <ol>
            <li>Upload your data set as a plaintext CSV spreadsheet file. The first row should contain a header giving the title of the variable recorded in that column, while each row thereafter represents an observation. The following columns are required: </li>
            <ul>
            <li>\"Species\", which holds the <i><u>species name</u></i> of the animal observation of that row. </li>
                  <li>\"Time\", which holds the <u>time of day</u> in which the animal was observed, in <b>decimal format</b>. It can be out of either 24 hours (e.g: 11:59pm -> 23.98) or 1 day (e.g: 11:59pm -> 0.99). </li>
                  <li>\"Date\", which holds the <u>date</u> on which the observation took place, in m/d/YYYY format. </li>
                  <li>\"Survey.Name\", which contains the <u>site</u> at which the study took place. Note that season names (\"Spring\", \"Summer\", \"Autumn\", \"Fall\", \"Winter\") and numbers (e.g: 2018) will be removed such that surveys can be grouped by name. For example, \"Site1 Summer 2017\", \"Site2 Fall 2010\", \"Spring 2008 Site1\", and \"Site2 2015 Autumn\" would be considered to come from \"Site1\", \"Site2\", \"Site1\", and \"Site2\", respectively. </li>
                  <li>\"Independent\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation). Rows with anything other than \"Yes\" in this column will be ignored. </li>
                  <li>\"X\", which holds the X coordinate of the observation when projected to UTM Zone 17N on WGS84 (EPSG:32617). </li>
                  <li>\"Y\", which holds the Y coordinate of the observation when projected to UTM Zone 17N on WGS84 (EPSG:32617). </li>
                </ul>
              <li>Select which independent variable you would like to use for your analysis. The three options are:</li>
                <ul>
                  <li>\"Clock Time\", which uses the time, exactly as recorded in the \"Time\" column of the uploaded data set, for all analyses. </li>
                  <li>\"Solar Time\", which converts the clock time to a \"Solar Time\" (expressed in terms of the sunrise and sunset) that is used for all analyses. </li>
                  <li>\"Moon Phase\", which uses the phase of the moon (calculated based on the date of the observation), rather than a time of day, for all analyses. </li>
                </ul>
              <li>Click the \"Data Analysis\" tab at the top of the page. This will bring you to a tool that is set up such that you can analyze the probability distributions generated for two groups, both of which can be filtered by species, site, and month. </li>
            </ol>"
          )
        )
      )
    ),
    tabPanel(title = "Data Analysis", #Data Analysis tab ----
      fluidRow(column(12,
                      titlePanel(""), #For spacing
                      HTML("Buttons, text boxes, and graphs on this page.")))
    )
  )
)