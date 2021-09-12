# TODO
# Map of suppliers
# Prices trend plot, better handling of multiple selections
# Future: location

#  # print list of input events (very useful for debugging the app, remove before deployment)
function(input, output){
  output$inputs <-
    renderPrint({reactiveValuesToList(input)})
  # materials table ####
  PoC_data <- readxl::read_excel("data/Dataset.xlsx", "Dataset") 
  
  #check
# PoC_data %>% count(Cost_Centre) %>% filter(n>1)
 
  #Sum per item id for calculating mean and range.
  material_table <- reactive({
    mat_table <- PoC_data %>% 
    group_by(CC_Group_Name,Item_Description) %>% 
      summarise(Number_of_Suppliers = n(), 
                Mean_Price_AUD = mean(Rate, na.rm=TRUE)) %>% 
      ungroup()
    if (is.null(input$filtGroups)) return(mat_table)
    mat_table %>% filter(CC_Group_Name %in% input$filtGroups)
  })
  
  #Prepare material table for web
  output$materialDT <- renderDT(server = FALSE,{
    DT::datatable(material_table() ,
                  colnames = gsub("_", " ", names(material_table())), rownames = FALSE, #formatting headers
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

  # material prices plot ####
  material_plot_data <- reactive({
    # selected_row <- ifelse(is.null(input$materialDT_rows_selected), 1, 
    #                        as.vector(input$materialDT_rows_selected))
    selected_row <- input$materialDT_rows_selected
    selected_material <- material_table()$Item_Description[selected_row] # 
    PoC_data %>% filter(Item_Description %in% selected_material)
  })
  material_price_dist <- function(){
    if (nrow(material_plot_data())==0) return(NULL)
    ggplot(material_plot_data(), 
           aes(x = Item_Description, y = Rate, fill = Item_Description)) + # 
      stat_halfeye(
        adjust = 0.5,
        justification = -.2,
        .width = 0,
        point_colour = NA) + 
      geom_boxplot(width = .25, outlier.color = NA, alpha = 0.5) +
      labs(y="Item Price (AUD)", x="Item") +
     # scale_y_continuous(breaks = int_breaks, limits = int_limits) + 
      # scale_fill_paletteer_d("awtools::a_palette") + 
      guides(fill="none") + 
      scale_fill_tq() + theme_tq() +
      coord_flip()
      # clean_theme$theme
      #      coord_flip() + clean_theme$theme
  }
  
   output$price_plot <- renderPlot(height = 300,{
     material_price_dist() +
       theme(plot.margin=margin(0,0,0,0, "mm")) # aspect.ratio = 1/2, 
     # ggsave(filedate("A_rabiei_pathogen_groups_2013-2020_horiz", ext = ".pdf",
     #                 outdir = "../output/", dateformat = FALSE),
     #        width =10, height = 8)
   })
  
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
        clean_names()
      
      #  Supplier_Location <- geocode(glue("{PoC_Supplier$preferred_name}, Australia") , 
      #                       output = "latlon", messaging=TRUE) %>% 
      #   mutate(preferred_name = PoC_Supplier$preferred_name) #%>% 
      # write_csv(Supplier_Location, "data/supplier_coordinates.csv")
      
      #get it from local
      Supplier_Location <- read_csv("data/supplier_coordinates2.csv")
      
      supplier_item_data <- reactive({
        selected_row <- input$materialDT_rows_selected
        selected_material <- material_table()$Item_Description[selected_row] # 
        PoC_data %>% filter(Item_Description %in% selected_material) %>% 
          left_join(., PoC_Supplier,by=c("Supplier_ID"="supplier_id")) %>% 
          left_join(Supplier_Location)
      })
      
      #Display table
      output$item_supplierDT <- renderDT(server = FALSE,{
        DT::datatable(supplier_item_data() %>% select(Item_Description, preferred_name,Unit, 
                                                      Rate, primary_contact_work_email),
                      #colnames = gsub("_", " ", names(material_table)), 
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
                        # columnDefs = list(list(width = '5%', targets = c(2))),
                        deferRender = TRUE, scrollY = 500,  scroller = TRUE))
      })
      
      #Display map
     output$SupplierMap <- renderLeaflet({
       leaflet(data = supplier_item_data()) %>% 
         addProviderTiles(provider = "OpenStreetMap") %>% 
         addMarkers(lng = ~lon, lat = ~lat)
     })
    
      
      
      
      
}
