


#----------------------------------Server.R-----------------------------
function(input, output, session) {
  #--------------------Agg plotdash---------- 
  output$plot1 <- renderPlotly({
    
    datakom <- data2 %>%
      filter(!Category %in% c("Average emission factor", "Share of electricity in total final energy consumption", "Average emission factor", "Total energy production")) %>% 
      group_by(Category, Year) %>%
      summarize(mean = round(mean(values, na.rm = TRUE), digits = 2)) %>%
      arrange(Category)
    #----------------- Mutate plotdash ----------------------
    # datakom <- datakom %>%
    #   mutate(labeling = glue ("Category : {Category}
    #                       Count : {total_values}"))
    #--------------------ggplot --------------------
    plot1 <- ggplot(datakom, aes(x = Year, y = mean, fill = Category)) +
      geom_area() + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
      theme_classic() +
      labs(title = "Global Production Comodity vs Consumtion Overtime",
           subtitle = "Energy Production Data",
           caption = "Source: Energdata.info",
           x = "Year", y = NULL) +
      theme(legend.position = "top", 
            plot.title = element_text(size = 15, hjust = 0.5),
            plot.subtitle = element_text(size = 8, hjust = 0.5)) +
      scale_fill_brewer(palette = "Blues")
    #--------------------ggplotly plotdash ---------------------
    # ggplotly(plot1, tooltip = "text")
    
    
  })

  
#--------------------Agg plot2----------
    output$plot2 <- renderPlotly({

      datacon_top10 <- data2 %>%
        filter(Category == input$Category) %>%
        group_by(Country, Category) %>% 
        summarise(Mean = round(mean(values), digits = 2)) %>% 
        ungroup() %>% arrange(-Mean) %>%  head(10)
#----------------- Mutate plot1 ----------------------
      datacon_top10 <- datacon_top10 %>% 
        mutate(labeling = glue ("Country : {Country}
                          Count : {Mean}"))
#--------------------ggplot --------------------
      plot2 <- ggplot(datacon_top10, aes(x = Mean, y = reorder(Country, Mean),text = labeling)) +
        geom_col(aes(fill = Mean)) + geom_hline(yintercept = 0) +
        scale_fill_gradient2(low = "#02293b", mid = "#94c3e0", high = "#b3d2e6", midpoint = median(datacon_top10$Mean)) + 
        labs(title = glue ("Top 10 Country {input$Category}"),
             subtitle = "Top 10 Energy Consumtion",
             caption = "Source: Energdata.info",
             x = NULL, y = NULL) +
        theme(legend.position = "bottom", 
              plot.title = element_text(size = 15, hjust = 0.5),
              plot.subtitle = element_text(size = 8, hjust = 0.5))
#--------------------ggplotly plot2 ---------------------
      ggplotly(plot2, tooltip = "text")


    })
    

    
#--------------------Agg plot3----------
    output$plot3 <- renderPlotly({
      
      dataconpro <- data2 %>%
        filter(!Category %in% c("Average emission factor","Coal and lignite production", "Natural gas production", "Refined oil products production", "Share of electricity in total final energy consumption", "Share of renewables in electricity production", "Share of wind and solar in electricity production", "Average emission factor")) %>% 
        group_by(Category, Year) %>%
        summarize(total_values = round(mean(values), digits = 2)) %>%
        arrange(Year,Category)
      #----------------- Mutate plot1 ----------------------
      dataconpro <- dataconpro %>% 
        mutate(labeling1 = glue ("Category : {Category}
                          Count : {total_values}
                          Year : {Year}"))
      #--------------------ggplot --------------------
      custom_colors <- c("Total energy consumption" = "#3885bb",  # Blue
                         "Total energy production" = "#bdd7e7")  # Grey
      
      # Create the plot with manual colors and points
      plot3 <- ggplot(dataconpro, aes(x = Year, y = total_values, color = Category, group = Category)) +
        geom_line(size = 1, alpha = 0.9, linetype = 1) +
        geom_point(aes(text = labeling1), size = 2 ) + 
        scale_color_manual(values = custom_colors) +# Add points for each line
        theme_minimal() +
        labs(title = "Global Energy Overtime",
             subtitle = "Production | Consumtion",
             caption = "Source: Energdata.info",
             x = NULL, y = NULL) +
        theme(legend.position = "top", 
              plot.title = element_text(size = 15, hjust = 0.5),
              plot.subtitle = element_text(size = 8, hjust = 0.5)) +
        scale_fill_brewer(palette = "Blues") 
      #--------------------ggplotly plot2 ---------------------
      ggplotly(plot3, tooltip = "text")
      
      
    })
#--------------------Agg plot4----------
    output$plot4 <- renderPlotly({
      
      datacatall <- data2 %>%
        filter(Category == input$Category) %>% 
        group_by(Country) %>% 
        summarise(Mean = round(mean(values), digits = 2)) %>% 
        ungroup() %>% 
        arrange(desc(Mean))
      #------------------ read ------------------------#
      shapefile_path <- "data_input/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp"
      world <- st_read(shapefile_path)
      #------------------ join ------------------------#
      map_data <- left_join(world, datacatall, by = c("NAME" = "Country"))
      #----------------- Mutate plot1 ----------------------
      map_data <- map_data %>% 
        mutate(Country = glue ("{NAME}"))
      #--------------------ggplot --------------------
      breaks <- pretty_breaks(n = 6)(map_data$Mean)
      plot4 <-
        ggplot(map_data) +
        geom_sf(aes(label = Country, fill = Mean)) +
        scale_fill_gradient2(name = NULL,
                             low = "#ffffcc", mid = "#bdd7e7", high = "#08519c",  
                             breaks = breaks) +
        labs(title = glue ("Global Energy | {input$Category}"),
             subtitle = NULL,
             caption = "Source: Energdata.info") +                          
        theme_minimal() +
        theme(
          # Legend
          legend.position = "top",                       
          legend.justification = 0.5,                   
          legend.key.size = unit(0.5, "cm"),            
          legend.key.width = unit(1, "cm"),           
          legend.text = element_text(size = 8),         
          legend.margin = margin(),                      
          # Judul dan Subjudul
          plot.title = element_text(size = 20, hjust = 0.5),  
          plot.subtitle = element_text(size = 8, hjust = 0.5)                
        )
      
      
    })
#--------------------Data Table ----------
output$data <- renderDataTable(
    data2,
    options = list(scrollx = T,
                  sccrolly = T))

}