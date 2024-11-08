library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(DT)
library(ggplot2)
library(maps)
library(maptools)
library(ggcorrplot)
library(ggtext)
library(leaflet)
library(highcharter)
library(data.table)
library(tidyverse)
library(reshape2)

ui <- dashboardPage(
  #Dashboard Title -----------
  dashboardHeader(title = "Ancaman Kawasan (SMART) FF IP Kalimantan Barat"),
  
  #Dashboard Header-----
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                #First menu item 
                menuItem(text = "Dataset", tabName = "Dataset", icon = icon("database")),
                menuItem(text = "Visualisasi", tabName = "Vis", icon = icon("chart-line")),
                # Conditional Panel for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar =='Vis' && input.t2 =='distro'",selectInput(inputId = "var1", label = "Kategori Temuan", choices = c1, selected = "Pembalakan")),
                conditionalPanel("input.sidebar =='Vis' && input.t2 =='trends'",selectInput(inputId = "var2", label = "Kategori Temuan", choices = c2, selected = "Kebakaran Hutan dan Lahan")),
                
                menuItem(text = "Map", tabName = "Map", icon = icon("map"))
    )
  ), 
  
  #Dashboard Body---------
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dataset", ##Tabitem Dataset----------
              #tabBox 1
              tabBox(id="t1", width=12,
                     tabPanel("Profil Area Kerja", icon = icon("address-card"), ### Tabpanel Profil Area Kerja-----
                              fluidRow(
                                column(width = 8, tags$img(src="Kalbar Area Project.jpg", width =550, height = 650, align= "center"),
                                       tags$br() , 
                                       tags$a("Profil Area Kerja FF IP Kalimantan Barat"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("Data ini merupakan kumpulan data patroli Ancaman Kawasan pada Area Kerja FF IP Kalimantan Barat
                                              Lanskap Kalimantan Barat terdiri dari 3 lanskap yaitu
                                              1. Lanskap Pegunungan Schwaner
                                              2. Lanskap Hutan Lahan Basah
                                              3. Lanskap Kaleutan")
                                       )
                              )
                     ),
                     tabPanel("Ringkasan Patroli", ###Tab Panel Ringkasan patroli --------
                              fluidRow(
                                column(width=12,
                                       div(
                                         h5(DT::DTOutput("dataRingkasan"),icon=icon("table"))
                                         )
                                       ),
                                column(width=11, height=5,
                                       div(
                                         h5(tabPanelBody("PanelRP", "Total Kegiatan Patroli Perkawasan per Tahun" , plotOutput("plotpanelR1"))))
                                       )
                              )
                              ),
                     tabPanel("Tipe Transportasi dan Jarak", DT::DTOutput("Transportasi"), icon = icon("table"), ###Tab Panel Tipe Transportasi---------
                              selectInput(inputId = "Transite", label = "Site", choices = sort(unique(tabelrp_cobs_type$Site)), selected = "Hutan Desa Tanjung Beulang"), 
                              sliderInput("Tranyears", "Tahun", min = min(pvt$Year), max = max(pvt$Year), value = c(2022)),
                              tabPanelBody("PanelTp", "Tipe Transportasi dan jarak per kawasan di setiap tahunnya", plotOutput("plotpanelT1"))),
                     tabPanel(title ="Aktivitas Manusia", DT::DTOutput("dataAncaman") ,icon=icon("table")), ###Tabitem Aktivitas Manusia-------------
                     
              )
      ),
      
      tabItem("Vis",##Tabitem Visualisasi------------
              
              tabBox(id ="t2", width=12,
                     tabPanel("Ancaman", value = "trends", ###tabpanel Ancaman--------
                              fluidRow(column(width = 12,
                                       div(
                                         tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1"), collapsible = TRUE, status = "primary", collapsed=TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2"), collapsible = TRUE, status = "primary", collapsed=TRUE, solidHeader = TRUE)))
                                       ),
                                       column(12,
                                              div(
                                                withSpinner(plotlyOutput("bar"))
                                              ))
                      )
                      ),
                     tabPanel("Distribution", value="distro", ### Tabpanel Distribusi---------
                               # selectInput("var", "Select the variable", choices=c("Rape", "Assault")),
                               withSpinner(plotlyOutput("histplot", height = "350px"))),
                     tabPanel("Korelasi", id="corr", plotlyOutput("cor")),
                     tabPanel("Hubungan Kategori Temuan", ### tabpanel Hubungan Kategori Temuan-----------
                               radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                               withSpinner(plotlyOutput("scatter")), value="relation",
                               conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "variabel x", choices = c2, selected = "Penggunaan Kawasan")),
                               conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'relation'", selectInput(inputId = "var4",label = "variabel y", choices = c2, selected = "Jarak", selectize = FALSE))
                              )
                     )
              ),
      
      tabItem(##Tabitem Maps---------
        tabName = "Map",
        box(selectInput(inputId = "Map",label = "Map Site", choices = c2, selected= "Pembalakan", multiple = TRUE,width = 250),
            withSpinner(plotOutput("map_plot")), width = 12
            )
        )
      )
    )
)

