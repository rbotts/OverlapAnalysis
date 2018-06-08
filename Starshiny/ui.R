#R Version 3.4.4
#Starshiny UI script

require("shiny")
require("shinymaterial")

#Constants ----
instructions <-
{"
<br>

<b>Instructions:</b>

<br>

<ol>
  <li>Upload your data set as a plaintext CSV spreadsheet file. The first row should contain a header giving the title of the variable recorded in that column, while each row thereafter represents an observation. The following columns are required:
    <ol>
      <li>\"Species\", which holds the <i><u>species name</u></i> of the animal observation of that row. </li>
      <li>\"Time\", which holds the <u>time of day</u> in which the animal was observed, in <b>decimal format</b>. It can be out of either 24 hours (e.g: 11:59pm -> 23.98) or 1 day (e.g: 11:59pm -> 0.99). </li>
      <li>\"Date\", which holds the <u>date</u> on which the observation took place, in m/d/YYYY format. </li>
      <li>\"Survey.Name\", which contains the <u>site</u> at which the study took place. Note that season names (\"Spring\", \"Summer\", \"Autumn\", \"Fall\", \"Winter\") and numbers (e.g: 2018) will be removed such that surveys can be grouped by name. For example, \"Site1 Summer 2017\", \"Site2 Fall 2010\", \"Spring 2008 Site1\", and \"Site2 2015 Autumn\" would be considered to come from \"Site1\", \"Site2\", \"Site1\", and \"Site2\", respectively. </li>
      <li>\"Independent\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation). Rows with anything other than \"Yes\" in this column will be ignored. </li>
      <li>\"X\", which holds the X coordinate of the observation when projected to UTM Zone 17N on WGS84 (EPSG:32617). </li>
      <li>\"Y\", which holds the Y coordinate of the observation when projected to UTM Zone 17N on WGS84 (EPSG:32617). </li>
    </ol>
  </li>
  
  <li>Click the \"Data Analysis\" tab at the top of this page. </li>

  <li>There will be 3 columns on that page. The first (leftmost) column contains the largest-scale, global options. These include the ability to select the independent variable to use for the analysis (explained below), the number of species to analyze (1 or 2), and which species to analyze. The second (and third, for two-group comparisons) column(s) give the ability to filter the data sets by species, site, and month.</li>
</ol>

<br>
        
<b>Independent Variables:</b> There are three different independent variables that you can use for your analysis:

<ol>
  <li>\"Clock Time\", which uses the time, exactly as recorded in the \"Time\" column of the uploaded data set, for all analyses. </li>
  <li>\"Solar Time\", which converts the clock time to a \"Solar Time\" (expressed in terms of the sunrise and sunset) that is used for all analyses. </li>
  <li>\"Moon Phase\", which uses the phase of the moon (calculated based on the date of the observation), rather than a time of day, for all analyses. </li>
</ol>
<br>
<br>
The source code for this app is available <u><a href=\"https://github.com/rbotts/OverlapAnalysis\">here</a></u> on Github.
"}

colHex <- "#f4511e"
colText <- "deep-orange darken-1"

#Page ----
material_page(
  title = "Starshiny: Overlap Analysis with R Shiny",
  nav_bar_color = colText,
  include_fonts = TRUE,
  
  material_tabs(
    tabs = c("Upload Data" = "upload",
             "Data Analysis" = "analysis",
             "About" = "about")
    ),
  
  #Upload tab ----
  material_tab_content(tab_id = "upload",
    material_row(
      material_column(
        width = 6,
        HTML("<br>This web app allows users study the patterns in and overlap between circular density curves. It is designed to be used to study the activity patterns of animals based on camera trap data, although it should also work with any other type of cyclically-dependent detection data.
             <br>
             <br>
             For more details and instructions for use, see the \"About\" tab.")
      ),
      material_column(
        width = 6,
        material_card(
          title = "", 
          HTML("Upload your data set as a plaintext CSV spreadsheet file."),
          material_file_input(input_id = "dataFile",
                              label = "Browse...",
                              color = colHex)
        )
      )
    )
    ),
  
  #Data Analysis tab ----
  material_tab_content(
    tab_id = "analysis",
    material_row(
      
      #First column, "global options"
      material_column(
        width = 4,
        material_card(
          title = "Global Options",
          
          #Mode selection radio buttons
          HTML("<p style = \"color:#9e9e9e\"> What would you like to use as your (x-axis) independent variable?"),
          material_radio_button(
            input_id = "modeSelect",
            label = NULL,
            choices = c("Clock Time" = "clock",
                        "Solar Time" = "solar",
                        "Moon Phase" = "lunar"),
            color = colHex
          ),
          
          #Number of species selction radio
          HTML("<br> <p style = \"color:#9e9e9e\"> How many species would you like to analyze?"),
          material_radio_button(
            input_id = "numberSpecies",
            label = NULL,
            choices = c("One" = 1,
                        "Two" = 2),
            color = colHex
          ),
          
          #Species selection dropdown box(es)
          uiOutput(outputId = "speciesSelect")
        )
      ),
      
      #Second/Third column(s), Filtration UI
      uiOutput(outputId = "filterUI")
    ),
    material_row(
      
      #Plot column/card
      material_column(
        width = 8,
        material_card(
          title = NULL,
          plotOutput(outputId = "overlapPlot",
                     width = "100%",
                     height = "480px")
        )
      ),
      
      #Data output column
      material_column(
        width = 4,
        
        #Basic Analysis card
        material_card(
          title = "Analysis...",
          uiOutput(outputId = "overlapResults")
        ),
        
        #Confidence Interval card
        material_card(
          title = "Overlap Confidence Interval",
          material_slider(
            input_id = "bootSlider",
            label = "Thousands of bootstrap samples",
            min_value = 1,
            max_value = 10,
            initial_value = 10,
            color = colHex
          ),
          material_button(
            input_id = "bootButton",
            label = "Calculate",
            color = colText
          ),
          uiOutput(outputId = "bootResults")
        )
      )
    )
  ),
  
  #About tab ----
  material_tab_content(
    tab_id = "about",
    
    material_row(
      material_column(
        width = 12,
        HTML(instructions)
      )
    )
  )
)
