main_tab <- tabItem(
  tabName = "materials",
  br(),
  br(),
  fluidRow(
    
    box(
      title = HTML("Select material"), 
      status = def_box_col, 
      width = 3, height = 220,
      solidHeader = FALSE, 
      collapsible = TRUE,
      pickerInput(
        inputId = "filtGroups",
        label = "Material Category", 
        choices = readxl::read_excel("data/Dataset_fake.xlsx", "Dataset_fake") %>% 
          select(CC_Group_Name) %>% arrange(CC_Group_Name) %>% 
          distinct() %>% .$CC_Group_Name,
        options = list(
          `live-search` = TRUE)
      ), # pickerInput
      uiOutput("filtMaterialControl"),
      uiOutput("filtPostcodeControl"),
      actionButton("go", "Submit")
      # pickerInput( # zip code
      # 
      #             ) # pickerInput
        ), # box
    box(
      title = HTML("Material prices"), 
      status = def_box_col, 
      width = 3, height = 220,
      solidHeader = FALSE, 
      collapsible = TRUE,
      # plotOutput("price_plot")
      valueBoxOutput("minPrice", width = 6),
      valueBoxOutput("maxPrice", width = 6), 
      valueBoxOutput("meanRating", width = 12)
      ), # box
    uiOutput("price_trend_UI")
 
  ), # fluidRow
  fluidRow(
    box(
      title = HTML("Suppliers map"), 
      width = 6, height = 600,
      status = def_box_col, 
      solidHeader = FALSE, 
      collapsible = TRUE,
      leafletOutput("SupplierMap", height = 450)
    ), # box
    box(width = 6, height = 600,
        title = HTML("Available suppliers"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
        DTOutput("item_supplierDT", height = 450)
    ) # box
  )# , # fluidRow
  # fluidRow(
  #   box(
  #     width = NULL,
  #     #height=400, 
  #     title = HTML("Inputs"), 
  #     status = def_box_col, 
  #     solidHeader = FALSE, 
  #     collapsible = TRUE,
  #     # plotOutput("price_plot"),
  #     verbatimTextOutput("inputs")
  #   ) # box
  # ) # fluidRow
) # tabItem