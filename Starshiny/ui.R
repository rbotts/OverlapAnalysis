#R Version 3.4.4
#Starshiny UI script

require("shiny")
require("shinymaterial")

#Constants ----
titleVersion <- "Starshiny 1.0"
instructions <- {"
<br>

<b>Instructions:</b>

<br>

<ol>
  <li>Upload your data set as a plaintext CSV spreadsheet file. The first row should contain a header giving the title of the variable recorded in that column, while each row thereafter represents an observation. The following columns are required:
    <ol>
      <li>\"Species\", which holds the <i><u>species name</u></i> of the animal observation of that row. </li>
      <li>\"Time\", which holds the <u>time of day</u> in which the animal was observed, in <b>decimal format</b>. It can be out of either 24 hours (e.g: 11:59pm -> 23.98) or 1 day (e.g: 11:59pm -> 0.99). </li>
      <li>\"Date\", which holds the <u>date</u> on which the observation took place, in m/d/YYYY format (e.g: May 4, 2010 -> 5/4/2010; November 1, 2016 -> 11/1/2016). </li>
      <li>\"Survey.Name\", which contains the <u>site</u> at which the study took place. Note that season names (\"Spring\", \"Summer\", \"Autumn\", \"Fall\", \"Winter\") and 20xx years (e.g: 2018) will be removed such that surveys can be grouped by name. For example, \"Site1 Summer 2017\", \"Site2 Fall 2010\", \"Spring 2008 Site1\", and \"Site2 2015 Autumn\" would be considered to come from \"Site1\", \"Site2\", \"Site1\", and \"Site2\", respectively. </li>
      <li>\"Independent\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation). Rows with anything other than \"Yes\" in this column will be ignored. </li>
      <li>\"Longitude\", which holds the <u>geographic longitude</u> of the observation in <b>decimal degrees</b>. </li>
      <li>\"Latitude\", which holds the <u>geographic latitude</u> of the observation in <b>decimal degrees</b>. </li>
    </ol>
  </li>
  
  <li>In the box prompting you to do so, enter the time zone your data uses. If it's the same as America/Costa_Rica (that is, UTC-6), you can skip this step; that is the default time zone assumed when nothing is typed into the text box. If you're unsure which time zone to use, pressing the button labelled \"Supported Timezones...\" will bring up a list of all timezones supported by the app. To close that list, scroll all the way to the bottom and click the button labelled \"Close\". </li>
  
  <li>Click the \"Data Analysis\" tab at the top of the page. </li>

  <li>There will be two rows on that page, with three columns in the first row and two columns in the second. Note that this page may take a few moments to fully load in, especially with large data sets.
    <ol>
      <li>The first (top-left) column contains the largest-scale, global options. These include the ability to select the number of species to analyze (1 or 2), the independent variable to use for the analysis (explained below), whether to throw out certain groups of data <i>(e.g: daytime data)</i>, and which species to analyze.</li>
      <li>The second and third columns give the ability to filter the data sets by site and month.</li>
      <li>The lower row will have a plot on the left, and some cards on the right. The top card will have basic analytics about the data. The sample size for each species will be shown, and, if analyzing in two-species comparison mode, some statistics regarding the degree of overlap or similarity between the two species, as well as a second card that allows estimation of related confidence intervals and p-values.</li>
  </li>
</ol>

<br>
        
<b>Independent Variables:</b> There are three different independent variables that you can use for your analysis:

<ol>
  <li>\"Clock Time\", which uses the time, exactly as recorded in the \"Time\" column of the uploaded data set, for all analyses. </li>
  <li>\"Solar Time\", which converts the clock time to a \"Solar Time\" (expressed in terms of the sunrise and sunset) that is used for all analyses. The times of sunrise/sunset are calculated based on the date, location (from Longitude and Latitude), and timezone in which each observation took place. </li>
  <li>\"Moon Phase\", which uses the phase of the moon (calculated based on the date of the observation), rather than a time of day, for all analyses. </li>
</ol>

<br>

<br>

The source code for this app is available <u><a href=\"https://github.com/rbotts/OverlapAnalysis\">here</a></u> on Github.
"}

timeZones <- OlsonNames()
timeZoneLengthTotal <- length(timeZones)
timeZoneLength1 <- trunc(timeZoneLengthTotal) / 2
timeZoneLength2 <- timeZoneLengthTotal - timeZoneLength1
timeZone1 <- head(timeZones, n = timeZoneLength1)
timeZone2 <- tail(timeZones, n = timeZoneLength2)

colHex <- "#f4511e"
colText <- "deep-orange darken-1"

#Page ----
material_page(
  title = titleVersion,
  nav_bar_color = colText,
  
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
        HTML("<br>
            <h4>Starshiny: Overlap Analysis with R Shiny</h4>
            <br>This web app allows users study the patterns in and overlap between circular density curves. It is designed to be used to study the activity patterns of animals based on camera trap data, although it should also work with any other type of cyclically-dependent detection data.
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
                              color = colHex),
          HTML("<br> In what time zone was your data recorded? (Default: \"America/Costa_Rica\")"),
          material_text_box(input_id = "timeZone",
                            label = NULL,
                            color = colHex),
          material_row(
            material_column(
              width = 6,
              offset = 6,
              align = "center",
              material_modal(modal_id = "timeZoneOptions",
                             button_text = "Supported Timezones...",
                             title = "Supported Timezones",
                             button_color = colText,
                             material_row(
                               material_column(
                                 width = 6, align = "left",
                                 material_card(
                                   title = NULL,
                                   HTML(paste(timeZone1, collapse = "<br>"))
                                 )
                               ),
                               material_column(
                                 width = 6, align = "left",
                                 material_card(
                                   title = NULL,
                                   HTML(paste(timeZone2, collapse = "<br>"))
                                 )
                               )
                             ))
            )
          )
        )
      )
    )
    ),
  
  #Data Analysis tab ----
  material_tab_content(
    tab_id = "analysis",
    
    #*First Row ----
    material_row(
      
      #First column, "global options"
      material_column(
        width = 4,
        material_card(
          title = "Global Options",
          material_row(
            #One/Two species switch
            material_column(
              width = 12, align = "center",
              material_switch(input_id = "speciesNumber",
                              label = NULL,
                              off_label = "Only A",
                              on_label = "A vs B",
                              initial_value = TRUE,
                              color = colHex)
            ),
            
            #Rest of the global UI
            material_column(
              width = 12,
              #Mode selection radio buttons
              HTML("<p style = \"color:#9e9e9e\"> What would you like to use as your (x-axis) independent variable?"),
              material_radio_button(
                input_id = "modeSelect",
                label = NULL,
                choices = c("Clock Time" = "TimeRad",
                            "Solar Time" = "Solar",
                            "Moon Phase" = "Lunar"),
                color = colHex
              ),
              
              #Choose data to ignore
              HTML("<p style = \"color:#9e9e9e\"> What data should be thrown out?"),
              material_checkbox(
                input_id = "removeDay",
                label = "Ignore daylight data (after sunrise, before sunset)",
                initial_value = FALSE,
                color = colHex
              ),
              material_checkbox(
                input_id = "removeNight",
                label = "Ignore nighttime data (after sunset, before sunrise)",
                initial_value = FALSE,
                color = colHex
              ),
              
              #Species selection dropdown box(es)
              uiOutput(outputId = "speciesSelect")
            )
          )
        )
      ),
      #Second/Third column(s), Filtering UI
      material_column(width = 8, uiOutput(outputId = "filterUI"))
    ),
    
    material_row(uiOutput(outputId = "loadingSpinner")),
    
    #*Second Row ----
    material_row(
      #Plot column/card
      material_column(
        width = 8,
        uiOutput(outputId = "plotCard")
      ),
      
      #Data output column
      material_column(
        width = 4,
        
        #Basic Analysis card
        uiOutput(outputId = "analysisCard"),
        
        #Confidence Interval card
        uiOutput(outputId = "bootCard")
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
