# TODO
# Map features: remove marker clustering, use filtering of PG from top menu
# Plots features: remove percentages from "Frequency of highly aggressive isolates" plot

function(input, output){
  # load("./data/patho_objects.RData")
  # load("./data/map_objects.RData") # load map objects created by samples_interactive_map
  # Reactive material filtration ####
 #  react_map_samples <- reactiveValues()
 #  react_map_samples$samples <- reactive({
 # #   show_samples <- vector(mode = "list", length = 2L) %>% set_names("samples", "icons")
 #    show_samples <- map_samples %>%
 #      filter(between(Path_rating,input$PG[1], input$PG[2]))
 #    if (input$PGna) {
 #      show_samples <- map_samples %>%
 #      filter(is.na(Path_rating)) %>% bind_rows(show_samples)
 #    }
 #    # show_samples$icons <- awesomeIcons(
 #    #     icon = 'leaf',
 #    #     iconColor = ifelse(show_samples$samples$Genotyped=="Y",'black','white' ),
 #    #     library = 'ion',
 #    #     markerColor = show_samples$samples$marker_cols)
 #    return(show_samples)
 #  })
 #  react_map_samples$map_icons <- reactive({
 #    awesomeIcons(
 #    icon = 'leaf',
 #    iconColor = ifelse(react_map_samples$samples()$Genotyped=="Y",'black','white' ),
 #    library = 'ion',
 #    markerColor = react_map_samples$samples()$marker_cols
 #    #markerColor = ifelse(is.na(map_samples$Path_rating), 'lightgray', 'white') # make this dependant on the pathogenicity
 #  )
 #  })
 #  
 #  # isolate_map <- reactive({
 #  #   
 #  # })
 #  # print list of input events (very useful for debugging the map/app)
  output$inputs <-
    renderPrint({reactiveValuesToList(input)})
 #  # Isolate map ####
 #  output$isolateMap <- renderLeaflet({
 #    leaflet(agro_zones_sf) %>%
 #      addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
 #      addProviderTiles("Esri.WorldTopoMap", group="Topo") %>% 
 #    addProviderTiles("Esri.WorldPhysical", group="Physical") %>% # causes issues with the labels
 #      addProviderTiles("Esri.NatGeoWorldMap", group="National Geographic") %>%
 #      setView(146.5, -30, zoom = 5) %>%  # -30.155307082559347, 146.48388609655257   ; central Australia: 133.7751, -27
 #      addProviderTiles("CartoDB.PositronOnlyLabels", group = "labels") %>%
 #      addAwesomeMarkers(
 #        lng = map_samples$lon,
 #        lat = map_samples$lat,
 #        popup = map_samples$popup_text,
 #        icon = icons,
 #        clusterOptions = markerClusterOptions(), group = "markers"
 #      )  %>%
 #      addScaleBar(position = "bottomright",
 #                  options = scaleBarOptions(imperial = FALSE)) %>%
 #      addEasyButton(easyButton(
 #        icon = "fa-globe",
 #        title = "Reset Map Zoom",
 #        onClick = JS("function(btn, map){ map.setView([-30, 146.5], 5); }")
 #      )) %>% 
 #      add_agro_layer(.) %>% 
 #      addLayersControl(
 #        position = "bottomright",
 #        baseGroups = c("Satellite", "Physical", "Topo",  "National Geographic"),
 #        overlayGroups = c("Agroecozones"), # , "labels"
 #        # "May Rain",
 #        # "June Rain",
 #        # "July Rain",
 #        # "August Rain"), 
 #        options = layersControlOptions(collapsed = TRUE)) %>%
 #      hideGroup("Agroecozones") 
 #  })
 #  # observe changes in the map
 #  # observe({
 #  #   LogMsg("Showing", input$PG, "PG filtering\n")
 #  # })
 #  observe({
 #    
 #  
 #    proxy <- leafletProxy("isolateMap")
 #    if (any(c("Satellite", "Physical") %in% input$isolateMap_groups)){
 #      # c("Satellite", "Physical") %in% c("Satellite", "labels",    "markers")
 #      proxy %>% showGroup("labels")
 #    } else {
 #      proxy %>% hideGroup("labels")
 #    }
 #    proxy %>% clearGroup("markers") %>% 
 #      addAwesomeMarkers(
 #        lng = react_map_samples$samples()$lon,
 #        lat = react_map_samples$samples()$lat,
 #        popup = react_map_samples$samples()$popup_text,
 #        icon = react_map_samples$map_icons(),
 #        clusterOptions = markerClusterOptions(), group = "markers"
 #      )  
 #    # Remove any existing legend, and only if the legend is
 #    # enabled, create a new one.
 #    # proxy %>% clearShapes() %>% removeTiles("agrozones") #%>% clearMarkers()
 #    # # proxy %>%
 #    # #   addAwesomeMarkers(
 #    # #     lng = react_map_samples()$lon,
 #    # #     lat = react_map_samples()$lat,
 #    # #     popup = ~ react_map_samples()$popup_text,
 #    # #     icon = proxy_icons,
 #    # #     clusterOptions = markerClusterOptions()
 #    # #   )
 #    # if ("zones" %in% input$checkmaplayers) {
 #    #   add_agro_layer(proxy)
 #    # }
 #  })
 #  # Zone summary table ####
 #  output$zoneSummary <- function() {
 #    zone_summary <- react_map_samples$samples() %>% 
 #      group_by(lat, lon) %>%
 #      summarise(
 #        site_samples = n(),
 #        Genotyped = sum(Genotyped == "Y", na.rm=TRUE),
 #        Phenotyped = sum(Path_rating>=0, na.rm =TRUE)
 #      ) %>%
 #      ungroup() %>%
 #      inner_join(site_summary %>% dplyr::select(lat,lon,Zone)) %>% 
 #      group_by(Zone) %>%
 #      summarise(Collected = sum(site_samples, na.rm = TRUE),
 #                Genotyped = sum(Genotyped, na.rm = TRUE),
 #                Phenotyped = sum(Phenotyped, na.rm = TRUE)
 #      ) %>% adorn_totals()
 #    kableExtra::kbl(zone_summary) %>% # rename(Genotyped = To_Genotype)) %>%
 #      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
 #                    font_size = 13) %>% 
 #      row_spec(nrow(zone_summary), bold = T)
 #  
 #  }
 #  
 #  # Host frequency plot ####    
 #  host_freq_plot <- function(){
 #    # define labels
 #    exlabel <- c("ICC3996", bquote("Genesis"^TM~"090")) # , "PBA~HatTrick~(PBR)", "PBA~Seamer~(PBR)")
 #    PBA_labels <- c("PBA HatTrick\n(PBR)", "PBA Seamer\n(PBR)")
 #    ggplot(data=host_response_data %>% filter(Diff_rating=="High"),
 #           aes(x=Year, y=freq, group = Diff_host, 
 #               colour=factor(Diff_host, levels = c("ICC3996", "Genesis090", "HatTrick", "Seamer")))) + 
 #      geom_line(size=1) + labs(y="Percentage of highly aggressive isolates", colour="Host") +
 #      scale_y_continuous(breaks = breaks_width(0.1),
 #                         labels = label_percent(accuracy = 1, suffix = ""),
 #                         expand = c(0,0), limits = c(0,1)) +
 #      # plot_theme(baseSize = 20) +
 #      scale_x_continuous(breaks=unique(host_response_data$Year)) +
 #      geom_text(data=host_response_data %>% filter(Diff_rating=="High",  Year!=2014, 
 #                                                   Year<2018,freq>0),
 #                aes(label=sprintf("%.1f%%", freq*100)), size=4, nudge_y = 0.015, 
 #                nudge_x = -0.15, colour="black") +
 #      geom_text(data=host_response_data %>% filter(Diff_rating=="High",  Year==2014,freq>0),
 #                aes(label=sprintf("%.1f%%", freq*100)), size=4, nudge_y = 0.03, 
 #                nudge_x = -0.15, colour="black") +
 #      geom_text(data=host_response_data %>% filter(Diff_rating=="High", Year==2018),
 #                aes(label=sprintf("%.1f%%", freq*100)), size=4, nudge_y = -0.015, 
 #                nudge_x = 0.2, colour="black") +
 #      geom_text(data=host_response_data %>% filter(Diff_rating=="High", Year==2020),
 #                aes(label=sprintf("%.1f%%", freq*100)),
 #                size=4, nudge_x = 0.3, colour="black") +
 #      scale_colour_paletteer_d("ggthemes::excel_Office_2007_2010", name = "", 
 #                               labels= c(bquote(.(exlabel)), PBA_labels)) +
 #      clean_theme$theme +
 #      theme(axis.title.y = element_text(size = rel(0.85), face = "plain"),
 #            legend.spacing.y = unit(1,"cm"), panel.grid.major = element_blank())
 #  }
 # 
 #  output$host_freq <- renderPlot(height = 400,host_freq_plot())
 #    # ggsave(filedate("A_rabiei_highly_aggressive_isolates_2013-2020", ext = ".pdf", 
 #    #                 outdir = "../output/", dateformat = FALSE), width = 10, height = 6)
 #  
 #  
 #  # Create the button to download the scatterplot as PDF
 #  output$downloadPlot1 <- downloadHandler(
 #    filename = function(plot_format=input$plotformat) {
 #      glue::glue('A_rabiei_isolate_frequency_{Sys.Date()}.{str_to_lower(plot_format)}')
 #    },
 #    content = function(file, plot_width = input$plotWidth, plot_height = input$plotHeight) {
 #      ggsave(file, host_freq_plot() + 
 #               theme(plot.margin=margin(t = 12, r=6, b=6, l=6, unit = "pt")),
 #             #   theme(plot.margin=margin(t = 1, r = 0.5, b = 0.5, l = 0.5, unit = "cm")), 
 #             width = plot_width, height = plot_height, 
 #             dpi = 300, units = "in")
 #    }
 #  )
 #  
  # materials table ####
  # tab_caption <- glue::glue("<i>Ascochyta rabiei</i> isolates collected as part of the GRDC projects GRI2007-001RTX and UM00052")
  # samples %>% dplyr::select(Isolate=ISOLATE_ID, Sample=SAMPLE_ID, STATE, LOCATION, Farm=FARM_ID,
  #                           Host=GENOTYPE) %>% rename_with(stringr::str_to_title) %>%
  #   left_join(patho_table %>% dplyr::select(-one_of(c("Pathotype", "Rating")))) %>%
  PoC_data <- readxl::read_excel("data/PoC_Data.xlsx") 
  
  # geodetails <- geocode(PoC_data$Supplier_Location , output = "latlon", messaging=TRUE) %>% 
  #   mutate(Supplier_Location = PoC_data$Supplier_Location) %>% 
  #   write_xlsx(., "data/PoC_Data.xlsx", sheet = "supp_coordinates")
  geodetails <- readxl::read_excel("data/PoC_Data.xlsx", sheet = "supp_coordinates")
  
  material_table <- PoC_data %>% group_by(Item_ID, Item_Description) %>% 
    summarise(Number_of_Suppliers = n(), Mean_Price_AUD = mean(Item_Price_AUD, na.rm=TRUE)) %>% 
    ungroup()
  
  
  output$materialDT <- renderDT(server = FALSE,{
    DT::datatable(material_table ,
                  colnames = gsub("_", " ", names(material_table)), rownames = FALSE,
                  style = 'bootstrap', class = 'table-bordered table-condensed',
                  extensions = c('Buttons','Scroller'),
                  #caption = htmltools::HTML(tab_caption),#
                  options = list(dom = 'ftSiB',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 deferRender = TRUE, scrollY = 500,  scroller = TRUE))
  })

  # material prices plot ####
  material_plot_data <- reactive({
    # selected_row <- ifelse(is.null(input$materialDT_rows_selected), 1, 
    #                        as.vector(input$materialDT_rows_selected))
    selected_row <- input$materialDT_rows_selected
    selected_material <- material_table$Item_Description[selected_row] # 
    PoC_data %>% filter(Item_Description %in% selected_material)
  })
  material_price_dist <- function(){
    if (nrow(material_plot_data())==0) return(NULL)
    ggplot(material_plot_data(), aes(x = Item_Price_AUD)) + # , fill = Item_Description
      geom_histogram(fill = "lightskyblue") + facet_wrap(~Item_Description, ncol = 1) +
      labs(y="Count", x="Item Price (AUD)") +
      # scale_fill_paletteer_d("awtools::a_palette") + 
      guides(fill="none") + 
      clean_theme$theme
      #      coord_flip() + clean_theme$theme
  }
  
   output$price_plot <- renderPlot(height = 300,{
     material_price_dist() +
       theme(aspect.ratio = 1/2, plot.margin=margin(0,0,0,0, "mm"))
     # ggsave(filedate("A_rabiei_pathogen_groups_2013-2020_horiz", ext = ".pdf",
     #                 outdir = "../output/", dateformat = FALSE),
     #        width =10, height = 8)
   })
  
  
 #  # Pathogenicity Group frequency plot ####  
 #  yearly_patho_groups <- function(){
 #    plot_data <- patho_sum %>% mutate(Patho_group=sub("Group", "", Pathotype))
 #    patho_labs <- plot_data %>% filter(Pathotype=="Group0") %>% mutate(label_pos=1-freq)
 #    ggplot(plot_data, aes(x=Year, y=freq, fill=Patho_group)) +
 #      geom_bar(stat = 'identity', width = 0.7) + labs(fill="Pathog. Group")  +
 #      scale_fill_brewer(palette = "Spectral", direction = -1) + 
 #      scale_x_continuous(breaks= as.integer(unique(plot_data$Year))) +
 #      geom_text(data =patho_labs, aes(x=Year, y=0.94, label=glue::glue("n={pop}")), 
 #                size=5, colour="white") + 
 #      # geom_text(data =patho_labs, aes(x=Year, y=label_pos, label=label), size=4.5, nudge_y = 0.065) +
 #      labs(y="Frequency") + 
 #      coord_flip() + clean_theme$theme
 #  }
 #  output$patho_plot <- renderPlot(height = 400,{
 #    yearly_patho_groups() + 
 #      theme(aspect.ratio = 1/2, plot.margin=margin(0,0,0,0, "mm"))
 #    # ggsave(filedate("A_rabiei_pathogen_groups_2013-2020_horiz", ext = ".pdf", 
 #    #                 outdir = "../output/", dateformat = FALSE), 
 #    #        width =10, height = 8)
 #  })
 #  
 #  # Create the button to download the scatterplot as PDF
 #  output$downloadPlot2 <- downloadHandler(
 #    filename = function(plot_format=input$plot2format) {
 #      glue::glue('A_rabiei_yearly_PG_frequency_{Sys.Date()}.{str_to_lower(plot_format)}')
 #    },
 #    content = function(file, plot_width = input$plot2Width, plot_height = input$plot2Height) {
 #      ggsave(file, yearly_patho_groups() + 
 #               theme(plot.margin=margin(t = 12, r=6, b=6, l=6, unit = "pt")), 
 #             width = plot_width, height = plot_height, 
 #             dpi = 300, units = "in")
 #    }
 #  )
}