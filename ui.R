dashboardPage(skin = "yellow",
  # md = TRUE,
  # skin = "midnight",
  dashboardHeader(
    controlbarIcon = icon(name="info-circle"), # lib = "glyphicon",
    fixed = TRUE,
    title = tagList(
      span(class = "logo-lg", "ESTI-MATE"), 
      img(src = "Estimate_Logo_Small.svg")), 
    #enable_rightsidebar = TRUE,
   # rightSidebarIcon = "gears", # gears
    leftUi = tagList()#,
   # userOutput("user")
  ),
  dashboardSidebar(
    id = "mainsidebar",
    sidebarMenu(
      menuItem(
        text = "Materials", 
        tabName = "materials",
       # badgeLabel = "new", 
       # badgeColor = "green",
        icon = icon(lib = "glyphicon", name = "home")
      ),
      menuItem(
        text = "Items by supplier", 
        tabName = "Suppliers",
        # badgeLabel = "new", 
        # badgeColor = "green",
        icon = icon( name = "tags")
      )#,
    ),
    hr(),
    pickerInput(
      inputId = "filtGroups",
      label = "Filter groups", 
      choices = readxl::read_excel("data/Dataset.xlsx") %>% select(CC_Group_Name) %>% arrange(CC_Group_Name) %>% distinct() %>% .$CC_Group_Name,
      options = list(
        `selected-text-format` = "count > 3"), 
      multiple = TRUE
    )

    # )
    # hr(),

    # skin selector
    # p(hr(),skinSelector(), style="margin-bottom:25cm")
  ),
  
  controlbar = dashboardControlbar(
    skin = "orange", #  "light", 
 #   icon = icon("info-circle"),

    controlbarMenu(
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
      data_tab,
      item_supplier_tab
    )
  ),
  title = HTML("ESTI-MATE"),

  footer = dashboardFooter(
    left = HTML("By Adi Gafni, Ido Bar..."),
    right = "Queensland University of Technology, 2021"
  )
)