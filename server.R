# TODO
# Map of suppliers
# Prices trend plot, better handling of multiple selections
# Future: location

#  # print list of input events (very useful for debugging the app, remove before deployment)
function(input, output){
  output$inputs <-
    renderPrint({reactiveValuesToList(input)})
  # materials table ####
  PoC_data <- readxl::read_excel("data/Dataset_fake.xlsx", "Dataset_fake") 
  
  # zip codes
  # con <- DBI::dbConnect(RSQLite::SQLite(), 
  #                       path = "data/australian-postcodes.db")
  # zip_codes <- dbListTables(con)
  #check
# PoC_data %>% count(Cost_Centre) %>% filter(n>1)
 
  # dynamic material selector
  output$filtMaterialControl <- renderUI({
    pickerInput(
      inputId = "filtMaterial",
      label = "Material", 
      choices = PoC_data %>% filter(CC_Group_Name %in% input$filtGroups) %>% 
        distinct(Item_Description) %>% pull(Item_Description),
      options = list(
        `live-search` = TRUE)
    ) # pickerInput
  }) # renderUI
  
  
  # Subcontractors table ####
  PoC_Subcontractors <- readxl::read_excel("data/Dataset.xlsx","Subcontractors") %>% 
    clean_names()
  
  # Subcont_Location <- geocode(glue("{PoC_Subcontractors$preferred_name}, Australia") , 
  #                     output = "latlon", messaging=TRUE) %>% 
  # mutate(preferred_name = PoC_Subcontractors$preferred_name) #%>% 
  # write_csv(Subcont_Location, "data/subcont_coordinates.csv")
  #get it from local
  Subcont_Location <- read_csv("data/subcont_coordinates.csv")
  
  
  PoC_Supplier <- readxl::read_excel("data/Dataset.xlsx","Supplier") %>% 
    clean_names() %>% 
    mutate(primary_contact_work_email = "admin@supplier.com",
           sustainability_rating = sample(1:5, nrow(PoC_Supplier), 
                                          replace = TRUE))
  
  #  Supplier_Location <- geocode(glue("{PoC_Supplier$preferred_name}, Australia") , 
  #                       output = "latlon", messaging=TRUE) %>% 
  #   mutate(preferred_name = PoC_Supplier$preferred_name) #%>% 
  # write_csv(Supplier_Location, "data/supplier_coordinates.csv")
  
  #get it from local
  Supplier_Location <- read_csv("data/supplier_coordinates2.csv") 
  
  # Aus postcodes
  aus_postcodes <- read_csv("data/australian_postcodes.csv") %>% 
    mutate(search_string = paste(postcode, 
                                 snakecase::to_any_case(locality,   "title"),
                                 state, sep = ", "))
  
  supplier_item_data <- reactive({
    # selected_row <- input$materialDT_rows_selected
    # selected_material <- material_table()$Item_Description[selected_row] # 
    PoC_data %>% filter(Item_Description %in% input$filtMaterial) %>% 
      left_join(., PoC_Supplier,by=c("Supplier_ID"="supplier_id")) %>% 
      left_join(Supplier_Location) %>% 
      mutate(popup_text = glue::glue(
        '<b>{preferred_name}</b><br/>Contact: {contact_name}, {primary_contact_work_email}') %>% map(~ HTML(.x)))
  })
  
  
  
  #Sum per item id for calculating mean and range.
  material_table <- reactive({
    mat_table <- PoC_data %>% 
    group_by(CC_Group_Name,Item_Description) %>% 
      summarise(Number_of_Suppliers = n(), 
                Mean_Price_AUD = mean(Rate, na.rm=TRUE), 
                Rate_min = min(Rate, na.rm = TRUE),
                Rate_max = max(Rate, na.rm = TRUE)) %>% 
      ungroup()
    if (is.null(input$filtGroups)) return(mat_table)
    mat_table %>% filter(CC_Group_Name %in% input$filtGroups)
  })
  
  #Prepare material table for web
  output$materialDT <- renderDT(server = FALSE,{
    DT::datatable(material_table() %>% select(-starts_with("Rat")) ,
                  colnames = gsub("_", " ", names(material_table() %>% select(-starts_with("Rate")))),
                                  rownames = FALSE, #formatting headers
                  style = 'bootstrap', class = 'table-bordered table-condensed',
                  extensions = c('Buttons','Scroller'),
                  options = list(initComplete = htmlwidgets::JS(
                            "function(settings, json) {",
                            "$(this.api().table().container()).css({'font-size': '85%'});", "}"),
                    dom = 'ftSiB',
                    sScrollXInner  = "60%",
                    scrollX = TRUE,
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    columnDefs = list(list(width = '5%', targets = c(2))),
                                 deferRender = TRUE, scrollY = 500,  scroller = TRUE))
  })

  # output$materialDT <- renderReactable({
  #   reactable(material_table() %>% clean_names("title"), searchable = TRUE, resizable = TRUE, pagination = FALSE, 
  #             showPageInfo = FALSE, selection = "multiple", onClick = "select", 
  #             borderless = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE, 
  #             showSortable = TRUE, height = 500, 
  #             columns = list(Sepal.Length = colDef(name = "Sepal Length", aggregate = "max", 
  #                                                  format = colFormat(suffix = " cm", digits = 1)), 
  #                            Sepal.Width = colDef(name = "Sepal Width", defaultSortOrder = "desc", aggregate = "mean", 
  #                                                 format = list(aggregated = colFormat(suffix = " (avg)", digits = 2))), 
  #                            Petal.Length = colDef(name = "Petal Length", aggregate = "sum"), 
  #                            Petal.Width = colDef(name = "Petal Width", aggregate = "count"), 
  #                            Species = colDef(aggregate = "frequency")))
  # })
  
  # material prices plot ####
  material_plot_data <- reactive({
    # selected_row <- ifelse(is.null(input$materialDT_rows_selected), 1, 
    #                        as.vector(input$materialDT_rows_selected))
    # selected_row <- input$materialDT_rows_selected
    # selected_material <- material_table()$Item_Description[selected_row] # 
    material_table() %>% filter(Item_Description %in% input$filtMaterial)
    # PoC_data %>% filter(Item_Description %in% selected_material)
  })
  
  # render KPI boxes ####
  # Max price
  output$maxPrice <- renderValueBox({
    valueBox(
      glue("${material_plot_data()$Rate_max}"),
      "Maximum price",
      icon = icon(name = "scale-unbalanced-flip"),
      color = "maroon"
    ) # valueBox
  }) # renderValueBox
  
  # Min price
  output$minPrice <- renderValueBox({
    valueBox(
      glue("${material_plot_data()$Rate_min}"),
      "Minimum price",
      icon = icon(name = "scale-unbalanced"),
      color = "teal"
    ) # valueBox
  }) # renderValueBox
  
  # Mean Rating
  output$meanRating <- renderValueBox({
    valueBox(
      glue("{mean(supplier_item_data()$sustainability_rating)}"),
      "Average sustainability rating",
      icon = icon(name = "leaf", lib = "glyphicon"),
      color = "green"
    ) # valueBox
  }) # renderValueBox
   
  
  # material_price_dist <- function(){
  #   if (nrow(material_plot_data())==0) return(NULL)
  #   ggplot(material_plot_data(), 
  #          aes(x = Item_Description, y = Rate, fill = Item_Description)) + # 
  #     stat_halfeye(
  #       adjust = 0.5,
  #       justification = -.2,
  #       .width = 0,
  #       point_colour = NA) + 
  #     geom_boxplot(width = .25, outlier.color = NA, alpha = 0.5) +
  #     labs(y="Item Price (AUD)", x="Item") +
  #    # scale_y_continuous(breaks = int_breaks, limits = int_limits) + 
  #     # scale_fill_paletteer_d("awtools::a_palette") + 
  #     guides(fill="none") + 
  #     scale_fill_tq() + theme_tq() +
  #     coord_flip()
  #     # clean_theme$theme
  #     #      coord_flip() + clean_theme$theme
  # }
  
  material_price_dist <- function(){
    if (nrow(material_plot_data())==0) return(NULL)
    ggplot(material_plot_data(), 
           aes(x = Item_Description)) + #
      geom_linerange(aes(ymin=Rate_min,ymax=Rate_max),linetype=1,
                     color="blue", size = 1.5)+
      geom_point(aes(y=Rate_min),size=3,color="red")+
      geom_point(aes(y=Rate_max),size=3,color="red")+
      scale_y_continuous(labels = dollar) + 
      scale_x_discrete(labels = NULL, breaks = NULL) + 
      labs(y="Price Range (AUD)", x="", 
           subtitle = HTML(glue("Price range of <b>{material_plot_data()$Item_Description}</b>"))) +
      # scale_y_continuous(breaks = int_breaks, limits = int_limits) + 
      # scale_fill_paletteer_d("awtools::a_palette") + 
      # guides(fill="none") + 
      scale_fill_tq() + theme_tq(base_size = 12) +
      # theme_ipsum_tw() + 
      theme(plot.subtitle = element_markdown(color = "black", size = 14)) +
      coord_flip()
    # clean_theme$theme
    #      coord_flip() + clean_theme$theme
  }
   output$price_plot <- renderPlot({
     material_price_dist() +
       theme(aspect.ratio = 1/2,plot.margin=margin(1,5,1,5, "mm")) # aspect.ratio = 1/2, 
     # ggsave(filedate("A_rabiei_pathogen_groups_2013-2020_horiz", ext = ".pdf",
     #                 outdir = "../output/", dateformat = FALSE),
     #        width =10, height = 8)
   })
  
   

      # dynamic material selector
      output$filtPostcodeControl <- renderUI({
        pickerInput(
          inputId = "filtPostcode",
          label = "Zoom map to postcode", 
          choices = aus_postcodes$search_string,
          options = list(
            `live-search` = TRUE)
        ) # pickerInput
      }) # renderUI
      
      
      
      # price trend plot
      price_trend <- function(){
        if (nrow(supplier_item_data())==0) return(NULL)
        plot_data <- supplier_item_data() %>% 
          select(Item_Description, Rate, Date)
        gg <- ggplot(plot_data, 
               aes(x = as.Date(Date), y = Rate)) +
          geom_line(size = 1.5, colour = "lightskyblue") +
          scale_y_continuous(labels = dollar, 
                             limits = c(0, max(plot_data$Rate)*1.2)) +
          scale_x_date(NULL, breaks = breaks_width("1 month"),
                       labels = label_date_short()) +
            labs(y="Price (AUD)") +
            # scale_y_continuous(breaks = int_breaks, limits = int_limits) + 
            # scale_fill_paletteer_d("awtools::a_palette") + 
            # guides(fill="none") + 
            # scale_fill_tq() + 
          theme_tq(base_size = 14) +
            # theme_ipsum_tw() + 
            theme(plot.subtitle = element_markdown(color = "black", size = 14),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank())
        return(gg)
        #   theme_ipsum_tw(base_size = 12) +
        #   theme_ipsum_ps(axis="xy")
        # flush_ticks(gg)
      }
      
      output$price_trend <- renderPlot({
        price_trend() +
          theme(plot.margin=margin(0,0,0,0, "mm")) # aspect.ratio = 1/2,
      })
      
      # dynamic price trend box selector
      output$price_trend_UI <- renderUI({
        box(
          
          title = HTML(glue("Price trend of <b>{input$filtMaterial}</b>")),
          status = def_box_col, 
          width = 6, height = 250,
          solidHeader = FALSE, 
          collapsible = TRUE,
          plotOutput("price_trend", height = 250)
        ) # box
      })
      
      #Display table
      output$item_supplierDT <- renderDT(server = FALSE,{
        DT::datatable(supplier_item_data() %>% 
                        select(Item_Description, preferred_name,Unit, 
                                Rate, sustainability_rating) %>% 
                        clean_names("title"),
                      #colnames = gsub("_", " ", names(material_table)), 
                      rownames = FALSE, #formatting headers
                      style = 'bootstrap', 
                      class = 'table-bordered table-condensed',
                      extensions = c('Buttons','Scroller'),
                      options = list(initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().container()).css({'font-size': '85%'});", "}"),
                        dom = 'ftSiB',
                        sScrollXInner  = "60%",
                        scrollX = TRUE,
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                        # columnDefs = list(list(width = '5%', targets = c(2))),
                        deferRender = TRUE, scrollY = 350,  scroller = TRUE))
      })
      
      
      
      #Display map
     output$SupplierMap <- renderLeaflet({
       leaflet(data = supplier_item_data()) %>% 
         addProviderTiles(provider = "OpenStreetMap") %>% 
         addMarkers(lng = ~lon, lat = ~lat, popup = ~popup_text)
     })
    
     
     # map proxy
     
     observeEvent(input$go, {
       map_centre  <- aus_postcodes %>% 
         filter(search_string %in% input$filtPostcode)
       leafletProxy("SupplierMap") %>%
         setView(map_centre$Long_precise, map_centre$Lat_precise, zoom = 10)
     })
     
      
      
      
      
}
