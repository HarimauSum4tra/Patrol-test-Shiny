

server <- function(input, output){
  #Data tabel menu item 1 tab panel 2, 3 ---------
  output$dataRingkasan <- renderDataTable(raw_tabelrp, filter = 'top',
                                          options = list(pageLength = 8, autoWidth = TRUE),
                                          fillContainer = getOption("DT.fillContainer", TRUE),
                                          class = 'multiple'
                                          )
  output$dataAncaman  <- renderDataTable(raw_tabelam, filter = 'top',
                                         options = list(pageLength = 8, autoWidth = TRUE),
                                         fillContainer = getOption("DT.fillContainer", TRUE),
                                         class = 'cell-border stripe',
                                         selection = 'multiple'
                                         )
  dft1 <- dcast( data = tabelrp_cobs_type, formula = Year + Patrol.Transport.Type ~ Site,  value.var = 'Jarak',  fun.aggregate = sum )  # get sum of V3 by grouping V1 and V2
  
  output$Transportasi <- renderDataTable(dft1, filter = 'top',
                                        options = list(pageLength = 8, autoWidth = TRUE),
                                        fillContainer = getOption("DT.fillContainer", TRUE),
                                        class = 'display',
                                        selection = 'multiple'
                                        )
  
  ### Output tingkat tabpanel body -------------
  ## panel body Ringkasan------
  output$plotpanelR1 <- renderPlot({

    df1 <- dcast( data = tabelrp_site, formula = Patrol.Year ~ Site,  value.var = 'n',  fun.aggregate = sum )  # get sum of V3 by grouping V1 and V2
    df1 <- melt( data = df1, id.vars = 'Patrol.Year')   # melt data
    df1 <- df1 %>% dplyr::rename(Site = variable, Year=Patrol.Year)
    
    ggplot(data = df1, aes( x = Year, y = value, fill = Site , group = Site) ) +    # print bar chart
      geom_bar( stat = 'identity', position = 'dodge' ) +
      geom_text(aes(label = value, y=value + 0.05), position = position_dodge(0.9), vjust=0)
  })


  output$plotpanelT1 <- renderPlot({
    
    
    data_plot <- subset(pvt2, year == input$Tranyears)
    
    ggplot(pvt2, aes(x= variable, y= values, fill= variable)) + 
      geom_col(position="dodge", width = 0.8)
 
    
  })
  
  # Untuk  - distribution charts pada menuitem 2 bisualisasi---------
  output$histplot <- renderPlotly({
    p1 = Var_korelasi2 %>% 
      plot_ly() %>% 
      add_histogram(x=~get(input$var1)) %>% 
      layout(xaxis = list(title = paste(input$var1)))
    
    
    p2 = Var_korelasi2 %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    ## stacking the plots on top of each other-----
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frekuensi"))
  })
  
  
  ## Scatter Charts pada menu item 2 visualisasi ---------
  output$scatter <- renderPlotly({
    p = Var_korelasi2 %>% 
      ggplot(aes(x=get(input$var3), y=get(input$var4))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("Korelasi b/w", input$var3 , "and" , input$var4),
           x = input$var3,
           y = input$var4) +
      theme(  plot.title = element_textbox_simple(size=10,
                                                  halign=0.5))
    
    
    # ggplotly : interaktif plot
    ggplotly(p)
    
  })
  
  
  
  
  ## Plot Korelasi pada menu item 2 visualisasi--------
  output$cor <- renderPlotly({
    my_df <- Var_korelasi2 %>% 
      select(-total.kegiatan,-Jarak, -Site, -Year)
    
    # Compute a correlation matrix
    corr <- round(cor(my_df), 2)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr, 
      method = c("circle"),
      show.legend = TRUE,
      hc.order = FALSE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
  
  
  # Server MenuItem 2 bar plot di bawahnya top low 5---------
  
  output$bar <- renderPlotly({
    ggplot(tabelam_site, aes(x = factor(Site), y = n, fill = as.character(input$var2))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Site", y = "Banyaknya Ancaman", fill = "Kategori Temuan") +
      facet_wrap(~Year, ncol = 2) +
      geom_text(aes(label = n, y= n + 0.05), position = position_dodge(0.1), vjust=0) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text.y = element_text(face = "italic")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      coord_flip()
    
  })
   
  #Server MenuItem 2 tabel top low 5---------
  output$head1 <- renderText(
    paste("Tertinggi Pada Temuan", input$var2)
  )
  output$head2 <- renderText(
    paste("Terendah Pada Temuan", input$var2)
  )
  
  
  ## Rendering top 5 tipe temuan ancaman-------
  output$top5 <- renderTable({
    
    Var_korelasi2 %>% select(Year, Site, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  ##  Rendering low 5 tipe temuan ancaman-------
  output$low5 <- renderTable({
    
    Var_korelasi2 %>% select(Year, Site, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
    
  })
  
  #Rendering MAP MENU ITEM 3 -----------
  
  data <- reactive({
    data.frame(
      name = kt_maps$Kategori.temuan,
      full_name = dataraw_id_am$ID,
      state = kt_maps$Site,
      lat = kt_maps$Y,
      lon = kt_maps$X,
    )%>%
      filter(state %in% input$Map)
  })
  output$Map <- renderHighchart({
    hcmap("custom/world", showLegend = FALSE) |>
      hc_add_series(
        data=data(),
        type = "mappoint",
        name = name,
        tooltip = list(
          pointFormat = "{point.name}" - "{point.full_name}Location: ({point.lat,.4f}, {point.lon:,.4f})"
        )
      )
  })
  
  
}

shinyApp(ui, server)
