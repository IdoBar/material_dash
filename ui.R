dashboardPage(
  # md = TRUE,
  # skin = "midnight",
  dashboardHeader(
    controlbarIcon = icon(name="info-circle"), # lib = "glyphicon",
    fixed = TRUE,
    title = tagList(
      span(class = "logo-lg", "MaterialPrices"), 
      img(src = "Giraffe_logo.svg")), # ShinyDashboardPlus_FINAL.svg
    #enable_rightsidebar = TRUE,
   # rightSidebarIcon = "gears", # gears
    leftUi = tagList(
      # dropdownBlock(
      #   id = "yearscroll",
      #   title = "Collection year",
      #   icon = icon("sliders"), badgeStatus = NULL,
      #   sliderInput(
      #     inputId = "year",
      #     label = "Collection year",
      #     min = 2013, 
      #     max = 2020, 
      #     value = c(2013,2020),
      #     sep = ""
      #   ),
      #   prettyToggle(
      #     inputId = "na",
      #     label_on = "NAs kept",
      #     label_off = "NAs removed",
      #     icon_on = icon("check"),
      #     icon_off = icon("remove")
      #   )
      # ),
      # dropdownBlock(
      #   id = "PGscroll",
      #   title = "Pathogenicity group",
      #   icon = icon("sliders"),
      #   badgeStatus = NULL,
      #   sliderInput(
      #     inputId = "PG",
      #     label = "Pathogenicity group",
      #     min = 0, 
      #     max = 5, 
      #     value = c(0,5)
      #   ),
      #   prettyToggle(
      #     inputId = "PGna",
      #     label_on = "Keep Non-phenotyped",
      #     label_off = "Discard Non-phenotyped",
      #     icon_on = icon("check"),
      #     value = TRUE,
      #     icon_off = icon("remove")
      #   )
      # )
    )#,
   # userOutput("user")
  ),
  dashboardSidebar(
    id = "mysidebar",
    sidebarMenu(
      menuItem(
        text = "Materials", 
        tabName = "materials",
       # badgeLabel = "new", 
       # badgeColor = "green",
        icon = icon(lib = "glyphicon", name = "home")
      )#,
      # menuItem(
      #   text = "Isolate Database", 
      #   tabName = "data_tables",
      #   # badgeLabel = "new", 
      #   # badgeColor = "green",
      #   icon = icon(lib = "glyphicon", name = "list-alt")
      # ),
      # menuItem(
      #   text = HTML("&ensp;Tutorial"), 
      #   tabName = "tutorial",
      #   # badgeLabel = "new", 
      #   # badgeColor = "green",
      #   icon = icon(lib = "font-awesome", name = "hire-a-helper")
      # ),
      # menuItem(
      #   text = "Research Team", 
      #   tabName = "team",
      #   # badgeLabel = "new", 
      #   # badgeColor = "green",
      #   icon = icon(lib = "glyphicon", name = "education")
      # )
    ),
    # hr()#,
    
#     h2("Information"),
#     HTML("The information provided in this dashboard has been collected and analysed as part of the <a href='https://grdc.com.au/'>GRDC</a> Project GRI2007-001RTX to monitor <i>Ascochyta rabiei</i> populations in the Australian chickpea growing regions. Data collection for this project has commenced in 2020 and had been added to the historical <i>A. rabiei</i> database that was collected in previous GRDC project <a href ='https://grdc.com.au/research/projects/project?id=2023'>UM00052</a>. 
# <br>
# A subset of the collected isolates was selected each year for phenotyping and genotyping to determine their pathogenicity on a differential set of chickpea cultivars and their genetic population structure."),
    hr(),
# add a reset button - see https://stackoverflow.com/a/34038254/5346827
    p(a(href = "javascript:history.go(0)", icon("repeat", lib = "glyphicon"), 
        HTML("&nbsp;&nbsp;Reset dashboard")),
      style="margin-left:5mm"),
# tags$style(HTML(".bottom_aligner {
#   display: inline-block;
#   height: 100%;
#   vertical-align: bottom;
#   width: 0px;
# }")),
   p(hr(),skinSelector(), style="margin-bottom:25cm")
  ),
  
  controlbar = dashboardControlbar(
    skin = "light", #  "light", 
 #   icon = icon("info-circle"),
#  div(
#  h2("Information"),
#  p(HTML("The information provided in this dashboard has been collected and analysed as part of the <a href='https://grdc.com.au/'>GRDC</a> Project GRI2007-001RTX to monitor <i>Ascochyta rabiei</i> populations in the Australian chickpea growing regions. Data collection for this project has commenced in 2020 and had been added to the historical <i>A. rabiei</i> database that was collected in previous GRDC project <a href ='https://grdc.com.au/research/projects/project?id=2023'>UM00052</a>.")),
# p("A subset of the collected isolates was selected each year for phenotyping and genotyping to determine their pathogenicity on a differential set of chickpea cultivars and their genetic population structure."),
#  style="margin-left: 1em; margin-right: 1em"),
    controlbarMenu(
      # controlbarItem(
      #   title = "Tab 1",
      #   icon = icon("desktop"),
      #   active = TRUE,
      #   sliderInput(
      #     inputId = "inputsidebar1",
      #     label = "Number of observations:",
      #     min = 0,
      #     max = 1000,
      #     value = 500
      #   )
      # ),
      # controlbarItem(
      #   icon = icon("paint-brush"),
      #   title = "Tab 2",
      #   numericInput(
      #     inputId = "inputsidebar2",
      #     label = "Observations:",
      #     value = 10,
      #     min = 1,
      #     max = 100
      #   )
      # )
    )
  ),
  dashboardBody(
    
    # use a bit of shinyEffects
    setShadow(class = "dropdown-menu"),
    setShadow(class = "box"),
    
    # some styling
    tags$head(
      tags$style(
        rel = "stylesheet",
        type = "text/css",
        href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
      ),
      tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
      ),
      tags$script(
        "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
      )
    ),
    ## CSS-Code ###############
    inlineCSS("
            #materialDT .table th {
             text-align: left;
            }

            #materialDT .table td {
             text-align: left;
            }
            "
    ),
    #####################
    
    # All tabs
    tabItems(
      # main_tab,
      data_tab#,
      # tutorial_tab,
      # team_tab
      # buttons_tab,
      # box_elements_tab,
      # extra_elements_tab
    )
  ),
  title = HTML("Materials Dashboard"),

  footer = dashboardFooter(
    left = HTML("By Adi Gafni, Ido Bar..."),
    right = "Queensland University of Technology, 2021"
  )
)